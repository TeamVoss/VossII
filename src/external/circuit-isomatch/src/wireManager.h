/**
 * Wire manager for a circuit
 *
 * Allocates fresh wire IDs, finds previously defined wire IDs to establish
 * connections, …
 */

#pragma once

#include <string>
#include <exception>
#include <vector>
#include <unordered_map>

#include "wireId.h"

class WireManager {
    public:
        /**
         * Thrown when trying to re-instantiate an already existing wire
         */
        class WireException : public std::exception {
            public:
                WireException(const char* wire) : wire(wire) {}
                const char* what() const throw () {
                    return wire;
                }
            private:
                const char* wire;
        };

        class AlreadyDefined : public WireException {
            using WireException::WireException;
        };
        class NotDefined : public WireException {
            using WireException::WireException;
        };

        /**
         * Basic constructor
         */
        WireManager();

        ~WireManager();

        /**
         * Allocates a fresh wire with the given name
         *
         * @throws AlreadyDefined
         */
        WireId* fresh(const std::string& name);

        /**
         * Checks the existence of a given wire
         */
        bool hasWire(const std::string& name);
        /**
         * Checks the existence of a given wire
         */
        bool hasWire(size_t id);

        /// Returns *all* wires, including the merged ones
        const std::vector<WireId*>& allWires() const { return wireById; }

        /// Returns a vector of unique wires
        std::vector<WireId*> wires() const;

        /**
         * Retrieves an existing wire, or creates it as a fresh one if it does
         * not exist yet.
         *
         * @param name The name to search
         * @param dontCreate If set to `true`, do not create the wire if it
         * does not exist, but raise NotDefined instead.
         */
        WireId* wire(const std::string& name, bool dontCreate=false);

        /**
         * Retrieves an existing wire by its id.
         *
         * @throws NotDefined if the given id does not exist
         */
        WireId* wire(size_t id);

        /** Renames a wire. Be sure to call this only if you do not rely on the
         * previous wire's name: the old name won't be mapped to that wire
         * anymore.
         * @throws NotDefined if there is no such wire. */
        void rename(const std::string& curName, const std::string& newName);

        /** Renames a wire. Be sure to call this only if you do not rely on the
         * previous wire's name: the old name won't be mapped to that wire
         * anymore.
         * @throws NotDefined if there is no such wire. */
        void rename(size_t id, const std::string& newName);

        /** Get this wire manager's unique id */
        size_t id() const { return id_; }

    private:
        std::vector<WireId*> wireById;
        std::unordered_map<std::string, WireId*> wireByName;

        static size_t nextId;
        size_t id_;
};

