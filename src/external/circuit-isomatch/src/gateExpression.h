/** Defines an AST for the expressions used in gates (combinator gates,
 * assertion gates).
 */

#pragma once

#include "signatureConstants.h"
#include <string>

namespace expr {

    /** Type of expression (used to cast to the right `struct`). */
    enum ExpressionType {
        ExprVar,        ///< End variable
        ExprConst,      ///< Integer constant
        ExprLongConst,  ///< Long integer constant
        ExprBinOp,      ///< Binary operator (+, AND, ...)
        ExprUnOp,       ///< Unary operator (NOT, ...)
        ExprUnOpCst,    ///< Unary operator with constant (CstLSL, ...)
        ExprSlice,      ///< Take a subword out of a word
        ExprMerge,      ///< Concatenate two words into a longer one
    };

    /***********************************************************************/
    /* WARNING! The following enums are duplicated (on purpose) inside the */
    /* C API. Change this API as well if you are to change these enums!    */
    /***********************************************************************/

    /** Operator for `ExprBinOp` */
    enum ExpressionBinOperator {
        BAnd,           ///< Bitwise and
        BOr,            ///< Bitwise or
        BXor,           ///< Bitwise exclusive or
        BAdd,           ///< Addition
        BSub,           ///< Subtraction
        BMul,           ///< Multiplication
        BDiv,           ///< Division
        BMod,           ///< Modulus
        BLsr,           ///< Logical shift right
        BLsl,           ///< Logical shift left
        BAsr,           ///< Arithmetic shift right
    };

    /** Operator for `ExprUnOp` */
    enum ExpressionUnOperator {
        UNot,           ///< Unary bitwise not
    };

    /** Operator for `ExprUnOpCst` */
    enum ExpressionUnOperatorCst {
        UCLsr,          ///< Logical shift right of fixed shift
        UCLsl,          ///< Logical shift left of fixed shift
        UCAsr,          ///< Arithmetic shift right of fixed shift
    };

};

class BadHex : public std::exception {};

/** Base expression type, inherited by every "real" expression type */
struct ExpressionBase {
    ExpressionBase(const expr::ExpressionType& type)
        : type(type), refcount(0) {}
    virtual ~ExpressionBase() {}
    expr::ExpressionType type;    ///< Type of the expression (used for casts)

    /** Compute a signature for this expression */
    virtual sign_t sign() const = 0; // FIXME memoize?

    /** Check whether two experessions are formally equal */
    bool equals(const ExpressionBase& oth) const;

    /// The object is referenced somewhere
    inline void addRef() {
        refcount++;
    }

    /// Call this instead of `delete`
    void deleteSelf() {
        refcount--;
        if(refcount == 0)
            delete this;
    }

    private:
        virtual bool innerEqual(const ExpressionBase& oth) const = 0;

    protected:
        int refcount;
};

/** Integer constant (`ExprConst`) */
struct ExpressionConst : ExpressionBase {
    ExpressionConst(unsigned val)
        : ExpressionBase(expr::ExprConst), val(val) {}

    unsigned val;           ///< Numeric value

    virtual sign_t sign() const;

    private:
        virtual bool innerEqual(const ExpressionBase& oth) const;
};

/** Integer long constant (`ExprLongConst`) */
struct ExpressionLongConst : ExpressionBase {
    ExpressionLongConst(const std::string& val)
        : ExpressionBase(expr::ExprConst), val(val)
    {
        for(const auto& ch: this->val) {
            if(!('0' <= ch && ch <= '9')
                    && !('a' <= ch && ch <= 'f')
                    && !('A' <= ch && ch <= 'F'))
                throw BadHex();
        }
    }

    std::string val;           ///< Numeric value

    virtual sign_t sign() const;

    private:
        virtual bool innerEqual(const ExpressionBase& oth) const;
};

/** End variable expression (`ExprVar`) */
struct ExpressionVar : ExpressionBase {
    ExpressionVar(int id) : ExpressionBase(expr::ExprVar), id(id) {}

    int id;                 ///< Id of the input pin referred

    virtual sign_t sign() const;

    private:
        virtual bool innerEqual(const ExpressionBase& oth) const;
};

/** Binary operator expression (`ExprBinOp`) */
struct ExpressionBinOp : public ExpressionBase {
    ExpressionBinOp(ExpressionBase* left,
            ExpressionBase* right,
            expr::ExpressionBinOperator op) :
        ExpressionBase(expr::ExprBinOp), left(left), right(right), op(op)
    {
        left->addRef();
        right->addRef();
    }
    virtual ~ExpressionBinOp() {
        left->deleteSelf();
        right->deleteSelf();;
    }

    ExpressionBase *left, *right;
    expr::ExpressionBinOperator op;       ///< Operator

    virtual sign_t sign() const;

    private:
        virtual bool innerEqual(const ExpressionBase& oth) const;
};

/** Unary operator expression (`ExprUnOp`) */
struct ExpressionUnOp : ExpressionBase {
    ExpressionUnOp(ExpressionBase* expr, expr::ExpressionUnOperator op) :
       ExpressionBase(expr::ExprUnOp), expr(expr), op(op)
    {
        expr->addRef();
    }
    virtual ~ExpressionUnOp() {
        expr->deleteSelf();
    }

    ExpressionBase *expr;           ///< Sub-expression
    expr::ExpressionUnOperator op;  ///< Operator

    virtual sign_t sign() const;

    private:
        virtual bool innerEqual(const ExpressionBase& oth) const;
};

/** Unary operator with constant (`ExprUnOpCst`) */
struct ExpressionUnOpCst : ExpressionBase {
    ExpressionUnOpCst(ExpressionBase* expr,
            int val,
            expr::ExpressionUnOperatorCst op) :
        ExpressionBase(expr::ExprUnOpCst), expr(expr), val(val), op(op)
    {
        expr->addRef();
    }
    virtual ~ExpressionUnOpCst() {
        expr->deleteSelf();
    }

    ExpressionBase *expr;
    int val;                            ///< Constant associated
    expr::ExpressionUnOperatorCst op;   ///< Operator

    virtual sign_t sign() const;

    private:
        virtual bool innerEqual(const ExpressionBase& oth) const;
};

/** Take a subword out of a word (`ExprSlice`) */
struct ExpressionSlice : ExpressionBase {
    ExpressionSlice(ExpressionBase* expr, unsigned beg, unsigned end) :
        ExpressionBase(expr::ExprSlice), expr(expr), beg(beg), end(end)
    {
        expr->addRef();
    }
    virtual ~ExpressionSlice() {
        expr->deleteSelf();
    }

    ExpressionBase *expr;
    unsigned beg;           ///< First index (inclusive) of the subword
    unsigned end;           ///< Last index (exclusive) of the subword

    virtual sign_t sign() const;

    private:
        virtual bool innerEqual(const ExpressionBase& oth) const;
};

/** Concatenate two words (`ExprMerge`) */
struct ExpressionMerge : ExpressionBase {
    ExpressionMerge(ExpressionBase* left, ExpressionBase* right) :
        ExpressionBase(expr::ExprMerge), left(left), right(right)
    {
        left->addRef();
        right->addRef();
    }
    virtual ~ExpressionMerge() {
        left->deleteSelf();
        right->deleteSelf();
    };

    ExpressionBase *left, *right;

    virtual sign_t sign() const;

    private:
        virtual bool innerEqual(const ExpressionBase& oth) const;
};
