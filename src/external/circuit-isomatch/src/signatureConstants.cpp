#include "signatureConstants.h"

namespace signatureConstants {
    typedef uint32_t u32;
    typedef uint64_t u64;

    uint64_t OperConstants::operator()(uint64_t v) const {
        uint32_t b1 = v;
        uint64_t out1 = (u64)(b1 + add) * (u64)lowMul;
        uint32_t b2 = (v >> 32) ^ (out1 >> 32);
        return (u64)(out1 % lowMod)
            | ((u64)((b2 + add) * highMul % highMod) << 32);
    }
}
