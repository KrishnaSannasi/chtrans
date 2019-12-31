//! The specification for the high level representation of types
//! 
//! [struct](crate::repr::r#struct) contains the specification for structs
//! 

use crate::hlist::*;
use crate::{Type, slots::{self, Uninit, SlotList}};
use typenum::{*, marker_traits::PowerOfTwo, operator_aliases::Mod};
use core::ops::{Rem, Sub};

/// specifies that the fields of a type must be laid out with `C`'s
/// field layout algorithm.
pub enum ReprC {}

/// specifies that the fields of a type must be laid out with in order
/// with no padding bytes
pub enum ReprPacked {}

pub mod r#struct;