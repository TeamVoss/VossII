#pragma once

#include "circuitTree.h"

class CircuitDelay : public CircuitTree {
    protected:
        // ========= I/O ITERATOR =============================================
        class InnerIoIter : public CircuitTree::InnerIoIter {
                WireId* ptr;
                const CircuitDelay* circ;
            public:
                InnerIoIter(const CircuitDelay* circ, WireId* wire)
                    : ptr(wire), circ(circ) {}
                InnerIoIter(const InnerIoIter& it)
                    : ptr(it.ptr), circ(it.circ) {}
                virtual void operator++();
                virtual WireId* operator*() { return ptr; }
                virtual InnerIoIter* clone() const {
                    return new InnerIoIter(*this);
                }
            protected:
                virtual bool equal(const CircuitTree::InnerIoIter& oth_) const
                {
                    const InnerIoIter& oth =
                        static_cast<const InnerIoIter&>(oth_);
                    return ptr == oth.ptr && circ == oth.circ;
                }
        };

    public:
        IoIter inp_begin() const {
            return IoIter(
                    new InnerIoIter(this, wireInput)
                    );
        }
        IoIter out_begin() const {
            return IoIter(
                    new InnerIoIter(this, wireOutput)
                    );
        }
        IoIter out_end() const {
            return IoIter(
                    new InnerIoIter(this, NULL)
                    );
        }
        // ========= END I/O ITERATOR =========================================

        CircuitDelay(WireId* from, WireId* to);

        CircType circType() const { return CIRC_DELAY; }

        /** Gets the input wire. */
        const WireId* input() const { return wireInput; }

        /** Gets the output wire. */
        const WireId* output() const { return wireOutput; }

        // Documentation in CircuitTree*
        size_t inputCount() const;
        size_t outputCount() const;
        WireId* nth_input(size_t circId) const;
        WireId* nth_output(size_t circId) const;

        void toDot(std::basic_ostream<char>& out, int indent=0);

    protected:
        virtual sign_t innerSignature() const;
        virtual bool innerEqual(CircuitTree* othTree);

    private:
        WireId *wireInput, *wireOutput;
};

