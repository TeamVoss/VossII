/**
 * Wire identifier for a circuit part
 *
 * This acts as a "in-between" graph node for connecting potentially many
 * circuit gates to a single wire. It has a name for convenience, and a list of
 * connected gates.
 */

#pragma once
#include <string>
#include <vector>
#include <unordered_set>
#include <exception>

// Circular inclusion
class CircuitTree;
class CircuitGroup;
class WireManager;
class IOPin;

class WireId {
	public:
        /** Connection to an IO pin */
        struct PinConnection {
            PinConnection(IOPin* pin, WireId* other) :
                pin(pin), other(other) {}
            IOPin* pin;
            WireId* other;
        };

        class CircIterator {
            typedef std::vector<CircuitTree*>::iterator CircIter;
            typedef std::vector<PinConnection>::iterator PinIter;

            public:
                CircIterator(const CircIter& iter, WireId* parent);
                CircIterator(const PinIter& iter, WireId* parent) :
                    pinIter(iter), isCircIter(false), parent(parent) {}

                CircIterator(const CircIterator& oth) {
                    operator=(oth);
                }
                void operator=(const CircIterator& oth) {
                    isCircIter = oth.isCircIter;
                    if(isCircIter)
                        circIter = oth.circIter;
                    else
                        pinIter = oth.pinIter;
                }

                CircIterator& operator++();

                CircuitTree* operator*() const;

                bool operator==(const CircIterator& oth) const {
                    return parent == oth.parent &&
                        isCircIter == oth.isCircIter &&
                        ((isCircIter && circIter == oth.circIter)
                         || (!isCircIter && pinIter == oth.pinIter));
                }
                bool operator!=(const CircIterator& oth) const {
                    return !operator==(oth);
                }


            private:
                union {
                    CircIter circIter;
                    PinIter pinIter;
                };
                bool isCircIter;
                WireId* parent;
        };

        class NoSuchConnection : public std::exception {};

		/**
		 * Basic constructor
		 *
		 * @param id Id of the wire
		 * @param name Convenience name for the wire
         * @param manager the `WireManager` used to create this wire
		 */
		WireId(size_t id, const std::string& name, WireManager* manager);

        ~WireId();

		/** Id-based equality */
		bool operator==(const WireId& oth) const;

		/** Id-based equality */
		bool operator==(WireId& oth);

        /// Id-based equality
		bool operator!=(const WireId& oth) const;
        /// Id-based equality
		bool operator!=(WireId& oth);

		/** Id-based comparaison */
		bool operator<(const WireId& oth) const;

		/** Id-based comparaison */
		bool operator<(WireId& oth);

        /** Connect a circuit to this wire. Should be handled by circuit
         * classes silently.
         */
        void connect(CircuitTree* circ);

        /** Connect a pin to this wire. */
        void connect(const PinConnection& pin);

        /** Connect a pin to this wire, creating a `PinConnection` on the fly.
         */
        void connect(IOPin* pin, WireId* other);

        /// Disconnect the given `CircuitTree`
        void disconnect(CircuitTree* circ);

        /// Disconnect the given `IOPin`
        void disconnect(IOPin* pin);

        /** Get the list of circuits connected to that wire. Fast. */
        const std::vector<CircuitTree*>& connectedCirc();

        /** Get the list of wires connected to that wire. Fast. */
        const std::vector<PinConnection>& connectedPins();

        /** Get the number of direct connections to this wire. */
        size_t connectedCount() {
            return connectedCirc().size() + connectedPins().size();
        }

        /** Get an iterator to the first adjacent circuit. Adjacent circuits
         * are the circuits directly connected to this wire. Groups connected
         * to this wire are considered (instead of considering the actual leaf
         * the wire is connected to inside this group).
         *
         * **NOTE**: the behaviour of this iterator is undefined when the
         * parent `WireId` is altered during the iteration. This includes
         * connecting more gates, merging this wire, … */
        CircIterator adjacent_begin();

        /** Get a past-the-end iterator to adjacent circuits for this group.
         * See `adjacent_begin`.  */
        CircIterator adjacent_end();

        /** Get the list of circuits connected to that wire, possibly through
         * other wires. Must perform a DFS through connected wires and create
         * the list on-the-fly, which might be a bit slow for heavy use. */
        std::vector<CircuitTree*> connected();

        /** Get the name of this wire */
        const std::string& name() { return inner()->name; }

        /** Get the name of this wire (const version) */
        const std::string& name() const { return inner()->name; }

        /** Get this wire's display unique name */
        std::string uniqueName();

	private:
        void walkConnected(std::unordered_set<CircuitTree*>& curConnected,
                std::unordered_set<WireId>& seenWires,
                WireId* curWire);

        struct Inner {
            size_t id;
            std::string name;
            WireManager* manager;
            std::vector<CircuitTree*> connected;
            std::vector<PinConnection> connectedPins;
        };

        void merge(WireId* other);
        void rename(const std::string& nName) { inner()->name = nName; }

        WireId* ufRoot();
        inline Inner* inner() { return ufRoot()->end; };
        const Inner* inner() const;

        union {
            Inner* end;
            WireId* chain;
        };
        bool isEndpoint;
        unsigned short ufDepth;

    friend WireManager; // set the wire's name
    friend struct std::hash<WireId>;
    friend struct std::hash<WireId*>;
    friend struct HashWirePtr;
};

namespace std {
    template<> struct hash<WireId> {
        typedef WireId argument_type;
        typedef std::size_t result_type;
        result_type operator()(const argument_type& wire) const {
            return wire.inner()->id;
        }
    };

    template<> struct hash<WireId*> {
        typedef WireId* argument_type;
        typedef std::size_t result_type;
        result_type operator()(const argument_type& wire) const {
            return wire->inner()->id;
        }
    };

    template<> struct equal_to<WireId*> {
        bool operator()(const WireId* lhs, const WireId* rhs) const
        {
            return *lhs == *rhs;
        }
    };
}
