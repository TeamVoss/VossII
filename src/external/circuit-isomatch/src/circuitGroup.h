#pragma once
#include <string>
#include <vector>
#include <exception>

#include "wireId.h"
#include "wireManager.h"
#include "circuitTree.h"
#include "subcircMatch.h"

class CircuitGroup;

/** Input/output pin for a `CircuitGroup` */
class IOPin {
    public:
        class AlreadyConnected : std::exception {};

        IOPin(WireId* formal, WireId* actual, CircuitGroup* group);

        /** Partially connect this pin, leave its outter part exposed for later
         * `connect`.
         */
        IOPin(std::string formalName, WireId* actual, CircuitGroup* group);

        /** Connects the outter part of the pin to the given wire.
         * @throw AlreadyConnected if the pin is already connected.
         */
        void connect(WireId* formal);

        WireId* formal() const { return _formal; }
        std::string formalName() const { return _formalName; }
        WireId* actual() const { return _actual; }
        CircuitGroup* group() const { return _group; }

    private:
        void link();

        WireId* _formal;
        std::string _formalName; /// Used if formal is not yet connected
        WireId* _actual;
        CircuitGroup* _group;

        friend CircuitGroup;
};


class CircuitGroup : public CircuitTree {
    protected:
        // ========= I/O ITERATOR =============================================
        class InnerIoIter : public CircuitTree::InnerIoIter {
                typedef std::vector<IOPin*>::const_iterator LowIter;
                LowIter ptr;
                const CircuitGroup* circ;
            public:
                InnerIoIter(const CircuitGroup* circ, LowIter lowIter)
                    : ptr(lowIter), circ(circ)
                {
                    nextValid();
                }
                InnerIoIter(const InnerIoIter& it)
                    : ptr(it.ptr) {}
                virtual void operator++();
                virtual WireId* operator*() { return (*ptr)->formal(); }
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
            private:
                void nextValid();
                void innerIncr();
        };

    public:
        IoIter inp_begin() const {
            return IoIter(
                    new InnerIoIter(this, grpInputs.begin())
                    );
        }
        IoIter out_begin() const {
            return IoIter(
                    new InnerIoIter(this, grpOutputs.begin())
                    );
        }
        IoIter out_end() const {
            return IoIter(
                    new InnerIoIter(this, grpOutputs.end())
                    );
        }
        // ========= END I/O ITERATOR =========================================

        /** Create a `CircuitGroup` with a given `name`. Its internal
         * `WireManager` is automatically created.
         */
        CircuitGroup(const std::string& name);

        /** Create a `CircuitManager` with a given `name` and `WireManager`.
         * The wire manager should **not** be shared with another
         * `CircuitGroup`, as this would mean they would share the same wire
         * naming scope.  The `manager` gets automatically `delete`d when the
         * group is de-allocated.
         */
        CircuitGroup(const std::string& name, WireManager* manager);

        /** Destroys the inner `WireManager`. */
        ~CircuitGroup();

        CircType circType() const { return CIRC_GROUP; }

        /**
         * Adds `child` as a child of this group. All external pins of `child`
         * are disconnected, and reconnected to this group's corresponding
         * wires during the process.
         */
        void addChild(CircuitTree* child);

        /**
         * Adds `pin` as input pin of this group.
         */
        void addInput(const IOPin& pin);

        /**
         * Adds an input pin to this group.
         */
        void addInput(const std::string& formal, WireId* actual);

        /**
         * Adds `pin` as output pin of this group.
         */
        void addOutput(const IOPin& pin);

        /**
         * Adds an output pin to this group.
         */
        void addOutput(const std::string& formal, WireId* actual);

        /**
         * Group's subcircuits, mutable.
         */
        std::vector<CircuitTree*>& getChildren();
        /**
         * Group's subcircuits
         */
        const std::vector<CircuitTree*>& getChildren() const;

        /// Groups's subcircuits, const version
        const std::vector<CircuitTree*>& getChildrenCst() const;

        /** Group's inputs, mutable.
         */
        std::vector<IOPin*>& getInputs();
        /** Group's inputs */
        const std::vector<IOPin*>& getInputs() const;

        /** Group's outputs, mutable.
         */
        std::vector<IOPin*>& getOutputs();
        /** Group's outputs */
        const std::vector<IOPin*>& getOutputs() const;

        /** Returns the I/O signature of a belonging to this group, that is, a
         * signature encompassing how this particular wire is connected to the
         * I/O pins of this group.
         */
        sign_t ioSigOf(WireId* id);

        /** Group's `WireManager`. */
        WireManager* wireManager() { return wireManager_; }
        // Note: this cannot be `const`, since the `wireManager_` is muted
        // whenever one tries to allocate a wire.

        /** Returns every match of `needle` found (recursively in the
         * hierarchy) in this group. Two results will never be overlapping; if
         * two overlapping subcircuits are matches, it is undefined which one
         * will be returned.
         */
        std::vector<MatchResult> find(CircuitGroup* needle);

        /// Get the group's name
        const std::string& name() const { return name_; }

        // Documentation in CircuitTree*
        size_t inputCount() const;
        size_t outputCount() const;
        WireId* nth_input(size_t circId) const;
        WireId* nth_output(size_t circId) const;

        virtual void unplug();

        void toDot(std::basic_ostream<char>& out, int indent=0);

    protected:
        void alteredChild();

        virtual sign_t innerSignature() const;
        virtual bool innerEqual(CircuitTree* othTree);
        void computeIoSigs();

    private:
        /// Thrown by `disconnectChild`
        class NoSuchChild: public std::exception {};

        void setAncestor(CircuitTree* tree) const;

        /** Removes the given circuit from this group's children. Trusts the
         * caller with calling `alter`. */
        void disconnectChild(CircuitTree* toRemove);

        std::string name_;

        WireManager* wireManager_;

        std::vector<CircuitTree*> grpChildren;
        std::vector<IOPin*> grpInputs, grpOutputs;

        memo_ts_t ioSigsTimestamp;
        std::unordered_map<WireId*, sign_t> ioSigs_;

    friend class CircuitTree;
};

