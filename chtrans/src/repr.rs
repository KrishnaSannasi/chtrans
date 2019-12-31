use crate::hlist::*;
use crate::{Type, slots::{self, Uninit, SlotList}};
use typenum::{*, marker_traits::PowerOfTwo, operator_aliases::Mod};
use core::ops::{Rem, Sub};

pub enum ReprC {}

pub mod r#struct;