// SPDX-License-Identifier: Apache-2.0

//! Array of `T` with dynamic allocations of `N` entries for slabs of size `M`
//!
//! The array will dynamically allocate slabs of size `M` to extend its internal
//! linked list, which will extend the capacity by `N`.
//!
//! Currently nightly until `#![feature(min_const_generics)]` hits stable with `1.51`.

#![deny(missing_docs)]
#![deny(clippy::all)]
#![cfg_attr(not(test), no_std)]

use core::alloc::GlobalAlloc;
use core::alloc::Layout;
use core::marker::PhantomData;
use core::mem::{align_of, size_of, MaybeUninit};
use core::ptr::NonNull;

#[inline]
const fn align_up<T>(addr: usize) -> usize {
    let align_mask = align_of::<T>() - 1;
    if addr & align_mask == 0 {
        addr // already aligned
    } else {
        (addr | align_mask) + 1
    }
}

/// Array of `T` with dynamic allocations of `N` entries for slabs of size `M`
pub struct SlabVec<T: Default + Copy + Sized, const N: usize, const M: usize> {
    head: Option<NonNull<ListPage<T, N, M>>>,
    tail: Option<NonNull<ListPage<T, N, M>>>,
    len: usize,
    alloc: &'static dyn GlobalAlloc,
}

impl<T: Default + Copy + Sized, const N: usize, const M: usize> SlabVec<T, N, M> {
    /// Number of entries a [`SlabVec`] could hold
    ///
    /// Trick used because of const_generics being still unstable.
    pub const NUM_ENTRIES: usize =
        (M - size_of::<ListPageHeader<T, N, 1>>()) / align_up::<T>(size_of::<T>());

    /// Get a new List using `alloc` as the allocator for its slabs
    pub fn new(alloc: &'static dyn GlobalAlloc) -> Self {
        let mut sl = Self {
            alloc,
            len: 0,
            head: None,
            tail: None,
        };
        sl.add_page();
        sl
    }

    /// Add one more page of size `M` with `N` entries
    pub fn add_page(&mut self) {
        let lp = NonNull::new({
            let ele = unsafe {
                &mut *(self
                    .alloc
                    .alloc(Layout::from_size_align(M, align_of::<ListPage<T, N, M>>()).unwrap())
                    as *mut MaybeUninit<ListPage<T, N, M>>)
            };

            unsafe {
                ele.as_mut_ptr().write(ListPage::<T, N, M> {
                    header: ListPageHeader { next: None },
                    entry: [T::default(); N],
                });
                // As the yet unstable MaybeUninit::assume_init_mut()
                &mut *ele.as_mut_ptr()
            }
        });

        if self.head.is_none() {
            self.head = lp;
        }

        if let Some(mut tail) = self.tail {
            unsafe { tail.as_mut() }.header.next = lp;
        }

        self.tail = lp;

        self.len += N;
    }

    /// Provides a forward iterator.
    pub fn iter(&self) -> Iter<'_, T, N, M> {
        Iter {
            current: self.head,
            index: 0,
            len: self.len,
            phantom: PhantomData,
        }
    }

    /// Provides a forward iterator with mutable references.
    pub fn iter_mut(&mut self) -> IterMut<'_, T, N, M> {
        IterMut {
            current: self.head,
            index: 0,
            len: self.len,
            phantom: PhantomData,
        }
    }
}

impl<T: Default + Copy + Sized, const N: usize, const M: usize> Drop for SlabVec<T, N, M> {
    fn drop(&mut self) {
        unsafe {
            let mut this = self.head.take();
            while let Some(mut ele) = this {
                this = ele.as_mut().header.next.take();
                self.alloc.dealloc(
                    ele.as_ptr() as *mut u8,
                    Layout::from_size_align(size_of::<Self>(), align_of::<Self>()).unwrap(),
                );
            }
        }
    }
}

#[repr(C)]
struct ListPageHeader<T: Default + Copy + Sized, const N: usize, const M: usize> {
    next: Option<NonNull<ListPage<T, N, M>>>,
}

#[repr(C)]
struct ListPage<T: Default + Copy + Sized, const N: usize, const M: usize> {
    header: ListPageHeader<T, N, M>,
    entry: [T; N],
}

/// An iterator over the elements of a `SlabVec`.
///
/// This `struct` is created by [`SlabVec::iter()`]. See its
/// documentation for more.
pub struct Iter<'a, T: Default + Copy + Sized, const N: usize, const M: usize> {
    current: Option<NonNull<ListPage<T, N, M>>>,
    index: usize,
    len: usize,
    phantom: PhantomData<&'a SlabVec<T, N, M>>,
}

impl<'a, T: Default + Copy + Sized, const N: usize, const M: usize> Iterator for Iter<'a, T, N, M> {
    type Item = &'a T;

    fn next(&mut self) -> Option<Self::Item> {
        let this: &ListPage<T, N, M> = unsafe { &*(self.current?.as_ptr()) };

        let ret = this.entry.get(self.index)?;

        self.len -= 1;
        self.index += 1;

        if self.index == N {
            self.current = this.header.next;
            self.index = 0;
        }
        Some(ret)
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.len, Some(self.len))
    }
}

/// A mutable iterator over the elements of a `SlabVec`.
///
/// This `struct` is created by [`SlabVec::iter_mut()`]. See its
/// documentation for more.
pub struct IterMut<'a, T: Default + Copy + Sized, const N: usize, const M: usize> {
    current: Option<NonNull<ListPage<T, N, M>>>,
    index: usize,
    len: usize,
    phantom: PhantomData<&'a mut SlabVec<T, N, M>>,
}

impl<'a, T: Default + Copy + Sized, const N: usize, const M: usize> Iterator
    for IterMut<'a, T, N, M>
{
    type Item = &'a mut T;

    fn next(&mut self) -> Option<Self::Item> {
        let this: &mut ListPage<T, N, M> = unsafe { &mut *(self.current?.as_ptr()) };

        let ret = this.entry.get_mut(self.index)?;

        self.len -= 1;
        self.index += 1;

        if self.index == N {
            self.current = this.header.next;
            self.index = 0;
        }

        Some(ret)
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.len, Some(self.len))
    }
}

#[cfg(test)]
mod tests {
    use crate::SlabVec;
    const PAGE_SIZE: usize = 4096;

    #[derive(Clone, Copy, Debug, Eq, PartialEq)]
    #[repr(align(16))]
    struct Data {
        a: i32,
        b: i32,
        c: u64,
    }

    impl Default for Data {
        fn default() -> Self {
            Self { a: -1, b: -1, c: 0 }
        }
    }

    type DataPageList = SlabVec<Data, { SlabVec::<Data, 1, PAGE_SIZE>::NUM_ENTRIES }, PAGE_SIZE>;

    #[test]
    fn len() {
        static mut SYSTEM: std::alloc::System = std::alloc::System;
        let pl = DataPageList::new(unsafe { &mut SYSTEM });
        assert_eq!(pl.iter().count(), 255);
    }

    #[test]
    fn const_len() {
        assert_eq!(DataPageList::NUM_ENTRIES, 255);
    }

    #[test]
    fn zero_len() {
        assert_eq!(
            SlabVec::<Data, { SlabVec::<Data, 1, 16>::NUM_ENTRIES }, 16>::NUM_ENTRIES,
            0
        );
    }

    #[test]
    fn iter() {
        static mut SYSTEM: std::alloc::System = std::alloc::System;
        let pl = DataPageList::new(unsafe { &mut SYSTEM });
        for entry in pl.iter() {
            assert_eq!(*entry, Data::default());
        }
    }

    #[test]
    fn iter_mut() {
        static mut SYSTEM: std::alloc::System = std::alloc::System;
        let mut pl = DataPageList::new(unsafe { &mut SYSTEM });

        for (i, entry) in pl.iter_mut().enumerate() {
            *entry = Data {
                a: i as _,
                b: i as _,
                c: i as _,
            };
        }

        for (i, entry) in pl.iter().enumerate() {
            assert_eq!(entry.a, i as _);
            assert_eq!(entry.b, i as _);
            assert_eq!(entry.c, i as _);
        }
    }
}
