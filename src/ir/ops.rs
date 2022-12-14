use phf::phf_map;
use pyo3::prelude::*;

use super::{py_string_to_enum, PhfStringMap};

macro_rules! op_enum {
    (
        $(#[$attr:meta])*
        $const_name:ident : $enum_name:ident {
            $($op:ident = $string:literal ,)*
        }
    ) => {
        $(#[$attr])*
        #[allow(non_camel_case_types)]
        #[derive(Clone, Copy, Debug)]
        pub enum $enum_name {
            $($op ,)*
        }

        const $const_name: PhfStringMap<$enum_name> = phf_map! {
            $($string => $enum_name::$op ,)*
        };

        impl<'source> FromPyObject<'source> for $enum_name {
            fn extract(ob: &'source PyAny) -> PyResult<Self> {
                py_string_to_enum(&$const_name, ob)
            }
        }
    };
}

op_enum! {
    /// Unary operation
    OPS1 : Op1 {
        Not8 = "Iop_Not8",
        Not16 = "Iop_Not16",
        Not32 = "Iop_Not32",
        Not64 = "Iop_Not64",
        Clz64 = "Iop_Clz64",
        Ctz64 = "Iop_Ctz64",
        _8Uto16 = "Iop_8Uto16",
        _8Uto32 = "Iop_8Uto32",
        _8Uto64 = "Iop_8Uto64",
        _16Uto32 = "Iop_16Uto32",
        _16Uto64 = "Iop_16Uto64",
        _32Uto64 = "Iop_32Uto64",
        _8Sto16 = "Iop_8Sto16",
        _8Sto32 = "Iop_8Sto32",
        _8Sto64 = "Iop_8Sto64",
        _16Sto32 = "Iop_16Sto32",
        _16Sto64 = "Iop_16Sto64",
        _32Sto64 = "Iop_32Sto64",
        _64to8 = "Iop_64to8",
        _64to16 = "Iop_64to16",
        _16to8 = "Iop_16to8",
        _32to16 = "Iop_32to16",
        _64to32 = "Iop_64to32",
        _64HIto32 = "Iop_64HIto32",
        _128to64 = "Iop_128to64",
        _128HIto64 = "Iop_128HIto64",
        _64to1 = "Iop_64to1",
        _1Uto8 = "Iop_1Uto8",
        _1Uto64 = "Iop_1Uto64",
        I32StoF64 = "Iop_I32StoF64",
        F32toF64 = "Iop_F32toF64",
        ReinterpF64asI64 = "Iop_ReinterpF64asI64",
        ReinterpI64asF64 = "Iop_ReinterpI64asF64",
        Sqrt32F0x4 = "Iop_Sqrt32F0x4",
        Sqrt64F0x2 = "Iop_Sqrt64F0x2",
        V128to64 = "Iop_V128to64",
        V128HIto64 = "Iop_V128HIto64",
        _64UtoV128 = "Iop_64UtoV128",
        _32UtoV128 = "Iop_32UtoV128",
        V256toV128_0 = "Iop_V256toV128_0",
        V256toV128_1 = "Iop_V256toV128_1",
    }
}

op_enum! {
    /// Binary operation
    OPS2 : Op2 {
        Add8 = "Iop_Add8",
        Add16 = "Iop_Add16",
        Add32 = "Iop_Add32",
        Add64 = "Iop_Add64",
        Sub8 = "Iop_Sub8",
        Sub16 = "Iop_Sub16",
        Sub32 = "Iop_Sub32",
        Sub64 = "Iop_Sub64",
        Mul8 = "Iop_Mul8",
        Mul16 = "Iop_Mul16",
        Mul32 = "Iop_Mul32",
        Mul64 = "Iop_Mul64",
        Or8 = "Iop_Or8",
        Or16 = "Iop_Or16",
        Or32 = "Iop_Or32",
        Or64 = "Iop_Or64",
        And8 = "Iop_And8",
        And16 = "Iop_And16",
        And32 = "Iop_And32",
        And64 = "Iop_And64",
        Xor8 = "Iop_Xor8",
        Xor16 = "Iop_Xor16",
        Xor32 = "Iop_Xor32",
        Xor64 = "Iop_Xor64",
        Shl8 = "Iop_Shl8",
        Shl16 = "Iop_Shl16",
        Shl32 = "Iop_Shl32",
        Shl64 = "Iop_Shl64",
        Shr8 = "Iop_Shr8",
        Shr16 = "Iop_Shr16",
        Shr32 = "Iop_Shr32",
        Shr64 = "Iop_Shr64",
        Sar32 = "Iop_Sar32",
        Sar64 = "Iop_Sar64",
        CmpEQ8 = "Iop_CmpEQ8",
        CmpEQ16 = "Iop_CmpEQ16",
        CmpEQ32 = "Iop_CmpEQ32",
        CmpEQ64 = "Iop_CmpEQ64",
        CmpNE8 = "Iop_CmpNE8",
        CmpNE16 = "Iop_CmpNE16",
        CmpNE32 = "Iop_CmpNE32",
        CmpNE64 = "Iop_CmpNE64",
        CasCmpEQ64 = "Iop_CasCmpEQ64",
        CasCmpNE8 = "Iop_CasCmpNE8",
        CasCmpNE16 = "Iop_CasCmpNE16",
        CasCmpNE32 = "Iop_CasCmpNE32",
        CasCmpNE64 = "Iop_CasCmpNE64",
        ExpCmpNE64 = "Iop_ExpCmpNE64",
        MullS8 = "Iop_MullS8",
        MullS32 = "Iop_MullS32",
        MullS64 = "Iop_MullS64",
        MullU32 = "Iop_MullU32",
        MullU64 = "Iop_MullU64",
        CmpLT32S = "Iop_CmpLT32S",
        CmpLT64S = "Iop_CmpLT64S",
        CmpLE32S = "Iop_CmpLE32S",
        CmpLE64S = "Iop_CmpLE64S",
        CmpLT32U = "Iop_CmpLT32U",
        CmpLT64U = "Iop_CmpLT64U",
        CmpLE32U = "Iop_CmpLE32U",
        CmpLE64U = "Iop_CmpLE64U",
        DivU8 = "Iop_DivU8",
        Mod8 = "Iop_Mod8",
        DivModU64to32 = "Iop_DivModU64to32",
        DivModS64to32 = "Iop_DivModS64to32",
        DivModU128to64 = "Iop_DivModU128to64",
        DivModS128to64 = "Iop_DivModS128to64",
        _32HLto64 = "Iop_32HLto64",
        _64HLto128 = "Iop_64HLto128",
        CmpF64 = "Iop_CmpF64",
        F64toI32S = "Iop_F64toI32S",
        F64toI64S = "Iop_F64toI64S",
        I64StoF64 = "Iop_I64StoF64",
        CosF64 = "Iop_CosF64",
        F64toF32 = "Iop_F64toF32",
        Add32x2 = "Iop_Add32x2",
        QSub8Sx8 = "Iop_QSub8Sx8",
        CmpGT32Sx2 = "Iop_CmpGT32Sx2",
        ShlN8x8 = "Iop_ShlN8x8",
        SarN8x8 = "Iop_SarN8x8",
        Perm8x8 = "Iop_Perm8x8",
        Add32F0x4 = "Iop_Add32F0x4",
        Sub32F0x4 = "Iop_Sub32F0x4",
        Mul32F0x4 = "Iop_Mul32F0x4",
        Div32F0x4 = "Iop_Div32F0x4",
        Max32F0x4 = "Iop_Max32F0x4",
        Min32F0x4 = "Iop_Min32F0x4",
        Add64F0x2 = "Iop_Add64F0x2",
        Sub64F0x2 = "Iop_Sub64F0x2",
        Mul64F0x2 = "Iop_Mul64F0x2",
        Div64F0x2 = "Iop_Div64F0x2",
        Max64F0x2 = "Iop_Max64F0x2",
        Min64F0x2 = "Iop_Min64F0x2",
        _64HLtoV128 = "Iop_64HLtoV128",
        SetV128lo64 = "Iop_SetV128lo64",
        SetV128lo32 = "Iop_SetV128lo32",
        AndV128 = "Iop_AndV128",
        OrV128 = "Iop_OrV128",
        XorV128 = "Iop_XorV128",
        Add32x4 = "Iop_Add32x4",
        Add64x2 = "Iop_Add64x2",
        ShlN32x4 = "Iop_ShlN32x4",
        ShlN64x2 = "Iop_ShlN64x2",
        ShrN32x4 = "Iop_ShrN32x4",
        InterleaveHI32x4 = "Iop_InterleaveHI32x4",
        InterleaveHI64x2 = "Iop_InterleaveHI64x2",
        InterleaveLO16x8 = "Iop_InterleaveLO16x8",
        InterleaveLO32x4 = "Iop_InterleaveLO32x4",
        InterleaveLO64x2 = "Iop_InterleaveLO64x2",
        V128HLtoV256 = "Iop_V128HLtoV256",
        AndV256 = "Iop_AndV256",
        OrV256 = "Iop_OrV256",
        XorV256 = "Iop_XorV256",
        Add32x8 = "Iop_Add32x8",
        ShlN32x8 = "Iop_ShlN32x8",
        ShrN32x8 = "Iop_ShrN32x8",
    }
}

op_enum! {
    /// Ternary operation
    OPS3 : Op3 {
        AddF64 = "Iop_AddF64",
        SubF64 = "Iop_SubF64",
        MulF64 = "Iop_MulF64",
        DivF64 = "Iop_DivF64",
    }
}
