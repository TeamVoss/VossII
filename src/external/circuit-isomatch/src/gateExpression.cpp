#include "gateExpression.h"
#include <exception>
#include <stdexcept>

using namespace signatureConstants;

class UnimplementedOperator : public std::exception {
    const char* what() {
        return "Reached the end of a supposedly comprehensive switch. "
            "An operator is unimplemented in `gateExpression.cpp`.";
    }
};

static bool isCommutative(expr::ExpressionBinOperator op) {
    switch(op) {
        case expr::BAnd:
        case expr::BOr:
        case expr::BXor:
        case expr::BAdd:
        case expr::BMul:
            return true;

        case expr::BSub:
        case expr::BDiv:
        case expr::BMod:
        case expr::BLsr:
        case expr::BLsl:
        case expr::BAsr:
            return false;
    }
    throw UnimplementedOperator();
}

static const OperConstants& cstOf(expr::ExpressionBinOperator op) {
    switch(op) {
        case expr::BAnd:
            return opcst_and;
        case expr::BOr:
            return opcst_or;
        case expr::BXor:
            return opcst_xor;
        case expr::BAdd:
            return opcst_add;
        case expr::BSub:
            return opcst_sub;
        case expr::BMul:
            return opcst_mul;
        case expr::BDiv:
            return opcst_div;
        case expr::BMod:
            return opcst_mod;
        case expr::BLsr:
            return opcst_lsr;
        case expr::BLsl:
            return opcst_lsl;
        case expr::BAsr:
            return opcst_asr;
    }
    throw UnimplementedOperator();
}

static const OperConstants& cstOf(expr::ExpressionUnOperator op) {
    switch(op) {
        case expr::UNot:
            return opcst_not;
    }
    throw UnimplementedOperator();
}

static const OperConstants& cstOf(expr::ExpressionUnOperatorCst op) {
    switch(op) {
        case expr::UCLsr:
            return opcst_un_lsr;
        case expr::UCLsl:
            return opcst_un_lsl;
        case expr::UCAsr:
            return opcst_un_asr;
    }
    throw UnimplementedOperator();
}

sign_t ExpressionConst::sign() const {
    return opcst_numconst(val);
}

sign_t ExpressionLongConst::sign() const {
    uint32_t hashed = 0;
    for(size_t pos=0; pos < val.size(); pos += 16) {
        uint32_t cVal = 0;
        for(int subpos = 0; pos + subpos < val.size()
                && subpos < 16; ++subpos)
        {
            char lVal = val[pos + subpos];
            if('0' <= lVal && lVal <= '9')
                lVal -= '0';
            else if('a' <= lVal && lVal <= 'f')
                lVal = lVal - 'a' + 10;
            else if('A' <= lVal && lVal <= 'F')
                lVal = lVal - 'A' + 10;
            else
                throw std::runtime_error("Bad hex (should not happen)");
            cVal <<= 4;
        }
        hashed ^= cVal;
    }
    return opcst_longconst(hashed);
}

sign_t ExpressionVar::sign() const {
    return opcst_wireid(id);
}

sign_t ExpressionBinOp::sign() const {
    if(isCommutative(op))
        return cstOf(op)(left->sign() + right->sign());
    return cstOf(op)(left->sign() - right->sign());
}

sign_t ExpressionUnOp::sign() const {
    return cstOf(op)(expr->sign());
}

sign_t ExpressionUnOpCst::sign() const {
    return cstOf(op)(expr->sign() - opcst_cstint(val));
}

sign_t ExpressionSlice::sign() const {
    return opcst_slice(expr->sign()
            - opcst_slicebounds(end * sliceMulInner - beg));
}

sign_t ExpressionMerge::sign() const {
    return opcst_merge(left->sign() - right->sign());
}

bool ExpressionBase::equals(const ExpressionBase& oth) const {
    if(type != oth.type)
        return false;
    return innerEqual(oth);
}

bool ExpressionConst::innerEqual(const ExpressionBase& oth) const {
    const ExpressionConst& o = dynamic_cast<const ExpressionConst&>(oth);
    return val == o.val;
}

bool ExpressionLongConst::innerEqual(const ExpressionBase& oth) const {
    const ExpressionLongConst& o =
        dynamic_cast<const ExpressionLongConst&>(oth);
    return val == o.val;
}

bool ExpressionVar::innerEqual(const ExpressionBase& oth) const {
    const ExpressionVar& o = dynamic_cast<const ExpressionVar&>(oth);
    return id == o.id;
}

bool ExpressionBinOp::innerEqual(const ExpressionBase& oth) const {
    const ExpressionBinOp& o = dynamic_cast<const ExpressionBinOp&>(oth);
    return op == o.op
        && left->equals(*o.left)
        && right->equals(*o.right);
}

bool ExpressionUnOp::innerEqual(const ExpressionBase& oth) const {
    const ExpressionUnOp& o = dynamic_cast<const ExpressionUnOp&>(oth);
    return op == o.op
        && expr->equals(*o.expr);
}

bool ExpressionUnOpCst::innerEqual(const ExpressionBase& oth) const {
    const ExpressionUnOpCst& o = dynamic_cast<const ExpressionUnOpCst&>(oth);
    return op == o.op
        && val == o.val
        && expr->equals(*o.expr);
}

bool ExpressionSlice::innerEqual(const ExpressionBase& oth) const {
    const ExpressionSlice& o = dynamic_cast<const ExpressionSlice&>(oth);
    return beg == o.beg && end == o.end && expr->equals(*o.expr);
}

bool ExpressionMerge::innerEqual(const ExpressionBase& oth) const {
    const ExpressionMerge& o = dynamic_cast<const ExpressionMerge&>(oth);
    return left->equals(*o.left) && right->equals(*o.right);
}
