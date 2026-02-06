//! Intermediate Representation

pub type IR = u64;

// Grows downard - leaves lower numbers for user defined functions.
pub const PUSH: IR = 0xffffffffffffff_ff;
pub const DUP: IR = 0xffffffffffffff_fe;
pub const ADD: IR = 0xffffffffffffff_fd;
pub const SUB: IR = 0xffffffffffffff_fc;
pub const MUL: IR = 0xffffffffffffff_fb;
pub const DIV: IR = 0xffffffffffffff_fa;
pub const RET: IR = 0xffffffffffffff_f9;
pub const HALT: IR = 0xffffffffffffff_f8;
