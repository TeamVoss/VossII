#pragma once

#include "circuitTree.h"
#include "gateExpression.h"

class CircuitComb : public CircuitTree {
    protected:
        // ========= I/O ITERATOR =============================================
        class InnerIoIter : public CircuitTree::InnerIoIter {
            public: typedef std::vector<WireId*>::const_iterator LowIter;
            private:
                LowIter ptr;
                const CircuitComb* circ;
            public:
                InnerIoIter(const CircuitComb* circ, LowIter lowIter);
                InnerIoIter(const InnerIoIter& it)
                    : ptr(it.ptr), circ(it.circ) {}
                virtual void operator++();
                virtual WireId* operator*() { return *ptr; }
                virtual InnerIoIter* clone() const {
                    return new InnerIoIter(*this);
                }
            protected:
                virtual bool equal(const CircuitTree::InnerIoIter& oth_) const
                {
                    const InnerIoIter& oth =
                        static_cast<const InnerIoIter&>(oth_);
                    return circ == oth.circ && ptr == oth.ptr;
                }
        };

    public:
        IoIter inp_begin() const {
            return IoIter(
                    new InnerIoIter(this, gateInputs.begin())
                    );
        }
        IoIter out_begin() const {
            return IoIter(
                    new InnerIoIter(this, gateOutputs.begin())
                    );
        }
        IoIter out_end() const {
            return IoIter(
                    new InnerIoIter(this, gateOutputs.end())
                    );
        }
        // ========= END I/O ITERATOR =========================================

        CircuitComb();
        virtual ~CircuitComb();

        CircType circType() const { return CIRC_COMB; }

        /// Adds `wire` as the next input for this gate.
        void addInput(WireId* wire);

        /** Adds `expr` as the expression for the next output wire, `out`. The
         * expression `expr` will be `delete`d by this object's destructor.
         */
        void addOutput(ExpressionBase* expr, WireId* wire);

        /** Gate's inputs */
        const std::vector<WireId*>& inputs() const { return gateInputs; }

        /** Gate's outputs */
        const std::vector<WireId*>& outputs() const { return gateOutputs; }

        /** Gate's expressions */
        const std::vector<ExpressionBase*>& expressions() const {
            return gateExprs;
        }

        // Documentation in CircuitTree*
        size_t inputCount() const;
        size_t outputCount() const;
        WireId* nth_input(size_t circId) const;
        WireId* nth_output(size_t circId) const;

        void toDot(std::ostream& out, int indent=0);

    protected:
        virtual sign_t innerSignature() const;
        virtual bool innerEqual(CircuitTree* othTree);

    private:
        std::vector<WireId*> gateInputs, gateOutputs;
        std::vector<ExpressionBase*> gateExprs;
};

