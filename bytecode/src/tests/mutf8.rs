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
use crate::mod_utf8::{modified_utf8_to_string, string_to_modified_utf8};

#[test]
fn test_to_modified_utf8_supplementary() {
    // 00000001_11110110_00000000
    let emoji = "\u{1F600}";
    // 11101101 10100000 10111101 11101101 10111000 10000000
    // 1101_100000_111101
    assert_eq!(
        string_to_modified_utf8(emoji),
        vec![0b11101101, 0b10100000, 0b10111101, 0b11101101, 0b10111000, 0b10000000]
    );
}

#[test]
fn test_from_modified_utf8_supplementary() {
    assert_eq!(
        modified_utf8_to_string(&[
            0b11101101, 0b10100000, 0b10111101, 0b11101101, 0b10111000, 0b10000000
        ])
        .unwrap(),
        "\u{1F600}"
    )
}

#[test]
fn test_to_modified_utf8_3bytes() {
    // 11111111_00110100
    let full_width_t = "Ｔ";
    // 11101111 10111100 10110100
    assert_eq!(
        string_to_modified_utf8(full_width_t),
        vec![0b11101111, 0b10111100, 0b10110100]
    )
}

#[test]
fn test_from_modified_utf8_3bytes() {
    assert_eq!(
        modified_utf8_to_string(&[0b11101111, 0b10111100, 0b10110100]).unwrap(),
        "Ｔ"
    )
}

#[test]
fn test_to_modified_utf8_2bytes() {
    let null = "\u{0000}";
    assert_eq!(string_to_modified_utf8(null), vec![0b11000000, 0b10000000]);
    // 00000011_10101001
    let omega = "Ω";
    assert_eq!(string_to_modified_utf8(omega), vec![0b11001110, 0b10101001]);
}

#[test]
fn test_from_modified_utf8_2bytes() {
    assert_eq!(
        modified_utf8_to_string(&[0b11000000, 0b10000000]).unwrap(),
        "\u{0000}"
    );
    assert_eq!(
        modified_utf8_to_string(&[0b11001110, 0b10101001]).unwrap(),
        "Ω"
    );
}

#[test]
fn test_to_modified_utf8_normal() {
    let alphabet = "abcde";
    assert_eq!(
        string_to_modified_utf8(alphabet),
        vec!['a', 'b', 'c', 'd', 'e']
            .iter()
            .map(|&c| c as u8)
            .collect::<Vec<u8>>()
    )
}
