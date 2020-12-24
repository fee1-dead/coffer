/*
    This file is part of Coffer.

    Coffer is free software: you can redistribute it and/or modify
    it under the terms of the GNU Lesser General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    Coffer is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU Lesser General Public License
    along with Coffer. (LICENSE.md)  If not, see <https://www.gnu.org/licenses/>.
*/

#[doc(hidden, reason = "implementation details")]
pub(crate) trait ByteSwap {
    #[inline]
    fn be_to_ne(&mut self) {
        #[cfg(target_endian = "little")]
        self.swap_bytes()
    }
    fn swap_bytes(&mut self);
}
impl ByteSwap for [u8; 1] {
    fn be_to_ne(&mut self) {}
    fn swap_bytes(&mut self) {}
}
impl ByteSwap for [u8; 2] {
    fn swap_bytes(&mut self) {
        self.swap(0, 1);
    }
}
impl ByteSwap for [u8; 4] {
    fn swap_bytes(&mut self) {
        self.swap(0, 3);
        self.swap(1, 2);
    }
}
impl ByteSwap for [u8; 8] {
    fn swap_bytes(&mut self) {
        self.swap(0, 7);
        self.swap(1, 6);
        self.swap(2, 5);
        self.swap(3, 4);
    }
}