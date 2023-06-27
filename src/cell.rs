use std::cell::UnsafeCell;
use std::fmt;

use gc_arena::barrier::Unlock;
use gc_arena::{Collect, Gc};

pub type GcCell<'gc, T> = Gc<'gc, TokenCell<T>>;

#[derive(Debug)]
pub struct Token(());

impl Token {
    /// # Safety
    /// This method is only safe to call if the resulting `Token` is never used
    /// to access a [`TokenCell`] that was accessed by another `Token`
    /// (unless the second token has already been dropped).
    ///
    /// In short, provided that there is only a single token active in a single
    /// scope at a time it is safe to use but using multiple tokens will rapidly
    /// lead to UB.
    pub unsafe fn new() -> Self {
        Self(())
    }

    /// Perform multiple disjoint borrows across a number of [`TokenCell`]s.
    ///
    /// # Panics
    /// Panics if the same reference is passed in multiple times. If `T` is a
    /// ZST then this may result in panics even if the cells are logically
    /// distinct.
    #[deprecated = "use MultiBorrow instead"]
    pub fn borrow_multi<'t, T, const N: usize>(
        &'t mut self,
        cells: [&'t TokenCell<T>; N],
    ) -> [&'t mut T; N] {
        let mut multi = MultiBorrow::new(self);
        std::array::from_fn(|i| multi.borrow_mut(&cells[i]))
    }
}

#[repr(transparent)]
pub struct TokenCell<T>(UnsafeCell<T>);

impl<T> TokenCell<T> {
    pub fn new(value: T) -> Self {
        Self(UnsafeCell::new(value))
    }

    pub fn into_inner(self) -> T {
        self.0.into_inner()
    }

    pub fn get_mut(&mut self) -> &mut T {
        self.0.get_mut()
    }

    pub fn borrow<'t>(&'t self, _token: &'t Token) -> &'t T {
        unsafe { self.borrow_unchecked() }
    }

    pub fn borrow_mut<'t>(&'t self, _token: &'t mut Token) -> &'t mut T {
        unsafe { self.borrow_unchecked_mut() }
    }

    pub unsafe fn borrow_unchecked(&self) -> &T {
        unsafe { &*self.0.get() }
    }

    pub unsafe fn borrow_unchecked_mut(&self) -> &mut T {
        unsafe { &mut *self.0.get() }
    }
}

impl<T> fmt::Debug for TokenCell<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // We can't print the value since it may be borrowed so we just print
        // TokenCell { .. } instead.
        f.debug_struct("TokenCell").finish_non_exhaustive()
    }
}

impl<T> Unlock for TokenCell<T> {
    type Unlocked = Self;

    unsafe fn unlock_unchecked(&self) -> &Self::Unlocked {
        self
    }
}

unsafe impl<'gc, T: Collect + 'gc> Collect for TokenCell<T> {
    fn needs_trace() -> bool {
        T::needs_trace()
    }

    fn trace(&self, cc: &gc_arena::Collection) {
        // SAFETY
        // ======
        // By itself, this is not sound. We are relying on the rest of the crate here to
        // not call TokenCell::trace while there is still a live reference from get().
        //
        // Luckily, this is pretty easy as long as the rest of this crate never calls
        // trace manually. Otherwise, trace is only called during a gc and gc_arena
        // ensures that we never access anything with a 'gc lifetime except during a
        // mutate() call. A gc cannot happen during a mutate() call so things should be
        // safe here.
        unsafe { self.borrow_unchecked_mut().trace(cc) }
    }
}

pub struct MultiBorrow<'a> {
    _token: &'a mut Token,
    bmut: Vec<*const ()>,
    bconst: Vec<*const ()>,
}

impl<'a> MultiBorrow<'a> {
    pub fn new(token: &'a mut Token) -> Self {
        Self {
            _token: token,
            bmut: Vec::new(),
            bconst: Vec::new(),
        }
    }

    /// Borrow `value` immutably using the current token.
    ///
    /// # Panics
    /// Panics if `value` has already been mutably borrowed.
    ///
    /// Note that the comparison is done based on cell address. If `T` is a
    /// zero-sized type then there may be multiple logical instances of `T` that
    /// share the same address. This method will still panic in that case.
    pub fn borrow<T>(&mut self, value: &'a TokenCell<T>) -> &'a T {
        assert!(
            !self.is_borrowed_mut(value),
            "call has already been mutably borrowed"
        );

        self.bconst.push(Self::as_ptr(value));

        // SAFETY: We just checked that the value is not already borrowed
        //         mutably and the fact that we have a mutable reference to the
        //         token ensures that there are no other borrows.
        unsafe { value.borrow_unchecked() }
    }

    /// Borrow `value` mutably using the current token.
    ///
    /// # Panics
    /// Panics if `value` has already been borrowed, mutably or immutably.
    ///
    /// Note that the comparison is done based on cell address. If `T` is a
    /// zero-sized type then there may be multiple logical instances of `T` that
    /// share the same address. This method will still panic in that case.
    pub fn borrow_mut<T>(&mut self, value: &'a TokenCell<T>) -> &'a mut T {
        assert!(!self.is_borrowed(value), "cell has already been borrowed");

        self.bmut.push(Self::as_ptr(value));

        // SAFETY: We just checked that the value is not already borrowed
        //         elsewhere. The token ensures that MultiBorrow is the
        //         only place allowed to create mutable borrows.
        unsafe { value.borrow_unchecked_mut() }
    }

    fn as_ptr<T>(value: &TokenCell<T>) -> *const () {
        value as *const TokenCell<T> as *const ()
    }

    fn is_borrowed_mut<T>(&self, value: &TokenCell<T>) -> bool {
        self.bmut.contains(&Self::as_ptr(value))
    }

    fn is_borrowed<T>(&self, value: &TokenCell<T>) -> bool {
        self.bconst.contains(&Self::as_ptr(value)) || self.is_borrowed_mut(value)
    }
}
