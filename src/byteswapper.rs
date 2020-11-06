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