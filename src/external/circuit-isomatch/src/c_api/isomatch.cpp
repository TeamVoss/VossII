/**
 * Implement the C api
 **/

#include "isomatch.h"
#include "../isomatch.h"

#include <exception>
#include <type_traits>
#include <set>
#include <cstring>

using namespace std;

/******************/
/* Internal state */
/******************/

struct MarkSweepState {
    std::set<CircuitTree*> roots;
    std::set<CircuitTree*> marks;
};

static MarkSweepState markSweepState;

static isom_rc lastError = ISOM_RC_OK;

/***************************/
/* Static helper functions */
/***************************/

class IsomError : public std::exception {
    public:
        IsomError(isom_rc error) : _error(error) {}
        const char* what() const noexcept { return isom_strerror(_error); }
        inline isom_rc error() const { return _error; }

    private:
        isom_rc _error;
};

static isom_rc handleError(const IsomError& err) {
    lastError = err.error();
    return lastError;
}

static CircuitTree* circuitOfHandle(circuit_handle handle) {
    if(handle == nullptr)
        throw IsomError(ISOM_RC_NULLPTR);

    CircuitTree* out = static_cast<CircuitTree*>(handle);
    return out;
}

template <typename Circ>
static Circ* circuitOfHandle(circuit_handle handle) {
    static_assert(std::is_base_of<CircuitTree, Circ>::value,
            "Template parameter must derive from CircuitTree");
    CircuitTree* circ = circuitOfHandle(handle);
    Circ* out = dynamic_cast<Circ*>(circ);
    if(out == nullptr)
        throw IsomError(ISOM_RC_DOMAIN);
    return out;
}

static ExpressionBase* exprOfHandle(expr_handle handle) {
    if(handle == nullptr)
        throw IsomError(ISOM_RC_NULLPTR);

    ExpressionBase* out = static_cast<ExpressionBase*>(handle);
    return out;
}

static WireId* wireOfHandle(wire_handle wire, CircuitGroup* context) {
    if(context == nullptr)
        throw IsomError(ISOM_RC_NO_PARENT);
    return context->wireManager()->wire(wire);
}

/** Sets the given `parent` as the gate's parent */
static void setParent(circuit_handle parent, CircuitTree* self) {
    CircuitGroup* par = circuitOfHandle<CircuitGroup>(parent);
    par->addChild(self);
}

/** Finds the root of a component, iterating `ancestor()`. */
CircuitTree* componentRootOf(CircuitTree* circ) {
    while(circ->ancestor() != nullptr)
        circ = circ->ancestor();
    return circ;
}

/** Internal function to free a `CircuitTree*`'s component. Returns the
 * iterator to the next `markSweepState.roots` element. */
static std::set<CircuitTree*>::iterator freeCircuitPtr(
       std::set<CircuitTree*>::iterator circuit)
{
    delete *circuit;
    auto nextRootIter = markSweepState.roots.erase(circuit);
    return nextRootIter;
}

/** Internal function to free a `CircuitTree*`'s component. Returns the
 * iterator to the next `markSweepState.roots` element. If `circuit`'s root is
 * somehow absent from the registered roots, `begin` is returned. */
static std::set<CircuitTree*>::iterator freeCircuitPtr(CircuitTree* circuit) {
    circuit = componentRootOf(circuit);
    auto circIter = markSweepState.roots.find(circuit);
    if(circIter == markSweepState.roots.end())
        throw IsomError(ISOM_RC_BAD_FREE);
    return freeCircuitPtr(circIter);
}

/** Same as `freeCircuitPtr`, but `delete`s the pointer without deleting the
 * root entry if there is no such entry (instead of failing with
 * `ISOM_RC_BAD_FREE`). */
static void freeCircuitPtr_nofail(CircuitTree* circ) {
    CircuitTree* root = componentRootOf(circ);
    auto rootIter = markSweepState.roots.find(root);
    if(rootIter != markSweepState.roots.end())
        freeCircuitPtr(rootIter);
    else
        delete circ;
}

/**********************/
/* API implementation */
/**********************/

isom_rc isom_last_error() {
    return lastError;
}

const char* isom_strerror(isom_rc err_code) {
    switch(err_code) {
        case ISOM_RC_OK:
            return "no error";
        case ISOM_RC_NULLPTR:
            return "bad handle (null pointer)";
        case ISOM_RC_DOMAIN:
            return "bad handle (cannot cast to right pointer type)";
        case ISOM_RC_NO_PARENT:
            return "this circuit handle has no parent group (see "
                "`build_group_add_child`) and is required to have one in this "
                "context. This applies eg. when you try to access a wire.";
        case ISOM_RC_BADHEX:
            return "the supplied hex string contains non-[0-9a-fA-F] char(s).";
        case ISOM_RC_BAD_FREE:
            return "the supplied pointer could not be found among registered "
                "groups.";
        case ISOM_RC_OUT_OF_RANGE:
            return "integer parameter out of range.";
        case ISOM_RC_NOT_CONNECTED:
            return "the requested pin is not connected to any wire.";
        case ISOM_RC_ERROR:
        default:
            return "generic error, please submit a bug report";
    }
}

// === Generic

int free_circuit(circuit_handle circuit) {
    try {
        freeCircuitPtr(circuitOfHandle(circuit));
        return ISOM_RC_OK;
    } catch(const IsomError& e) {
        return handleError(e);
    }
}

int isom_unplug_circuit(circuit_handle circuit) {
    try {
        CircuitTree* circ = circuitOfHandle(circuit);
        circuitOfHandle(circ)->unplug();
        freeCircuitPtr_nofail(circuitOfHandle(circuit));
        return ISOM_RC_OK;
    } catch(const IsomError& e) {
        return handleError(e);
    }
}

// === Assert

circuit_handle build_assert(circuit_handle parent,
        const char* name,
        expr_handle expr)
{
    try {
        CircuitAssert* out = new CircuitAssert(name, exprOfHandle(expr));
        setParent(parent, out);
        return out;
    } catch(const IsomError& e) {
        handleError(e);
        return nullptr;
    }
}

int build_assert_add_input(circuit_handle self, wire_handle wire) {
    try {
        CircuitAssert* asser = circuitOfHandle<CircuitAssert>(self);
        asser->addInput(wireOfHandle(wire, asser->ancestor()));
        return ISOM_RC_OK;
    } catch(const IsomError& e) {
        return handleError(e);
    }
}

// === Comb

circuit_handle build_comb(circuit_handle parent) {
    try {
        CircuitComb* out = new CircuitComb();
        setParent(parent, out);
        return out;
    } catch(const IsomError& e) {
        handleError(e);
        return nullptr;
    }
}

int build_comb_add_input(circuit_handle self, wire_handle wire) {
    try {
        CircuitComb* circ = circuitOfHandle<CircuitComb>(self);
        circ->addInput(wireOfHandle(wire, circ->ancestor()));
        return ISOM_RC_OK;
    } catch(const IsomError& e) {
        return handleError(e);
    }
}

int build_comb_add_output(circuit_handle self,
        wire_handle wire,
        expr_handle expr)
{
    try {
        CircuitComb* circ = circuitOfHandle<CircuitComb>(self);
        circ->addOutput(exprOfHandle(expr),
                wireOfHandle(wire, circ->ancestor()));
        return ISOM_RC_OK;
    } catch(const IsomError& e) {
        return handleError(e);
    }
}

// === Delay

circuit_handle build_delay(circuit_handle parent,
        wire_handle input,
        wire_handle output)
{
    try {
        CircuitGroup* par = circuitOfHandle<CircuitGroup>(parent);
        CircuitDelay* delay = new CircuitDelay(
                wireOfHandle(input, par),
                wireOfHandle(output, par));
        setParent(parent, delay);
        return delay;
    } catch(const IsomError& e) {
        handleError(e);
        return nullptr;
    }
}

// === Group

circuit_handle build_group(const char* name) {
    try {
        CircuitGroup* out = new CircuitGroup(name);
        markSweepState.roots.insert(out);
        return out;
    } catch(const IsomError& e) {
        handleError(e);
        return nullptr;
    }
}

int build_group_add_child(circuit_handle self, circuit_handle child) {
    try {
        CircuitGroup* group = circuitOfHandle<CircuitGroup>(self);
        CircuitTree* childPtr = circuitOfHandle(child);
        group->addChild(childPtr);
        auto childRoot = markSweepState.roots.find(childPtr);
        if(childRoot != markSweepState.roots.end())
            markSweepState.roots.erase(childRoot);
        return ISOM_RC_OK;
    } catch(const IsomError& e) {
        return handleError(e);
    }
}

int build_group_add_input(circuit_handle self,
        wire_handle actual,
        wire_handle formal)
{
    try {
        CircuitGroup* group = circuitOfHandle<CircuitGroup>(self);
        if(group->ancestor() != nullptr) {
            group->addInput(IOPin(
                        wireOfHandle(formal, group->ancestor()),
                        wireOfHandle(actual, group),
                        group));
        }
        else
            group->addInput(formal, wireOfHandle(actual, group));

        return ISOM_RC_OK;
    } catch(const IsomError& e) {
        return handleError(e);
    }
}

int build_group_add_output(circuit_handle self,
        wire_handle actual,
        wire_handle formal)
{
    try {
        CircuitGroup* group = circuitOfHandle<CircuitGroup>(self);
        if(group->ancestor() != nullptr) {
            group->addOutput(IOPin(
                        wireOfHandle(formal, group->ancestor()),
                        wireOfHandle(actual, group),
                        group));
        }
        else
            group->addOutput(formal, wireOfHandle(actual, group));

        return ISOM_RC_OK;
    } catch(const IsomError& e) {
        return handleError(e);
    }
}

// === Tristate

circuit_handle build_tristate(circuit_handle parent,
        wire_handle from,
        wire_handle to,
        wire_handle enable)
{
    try {
        CircuitGroup* par = circuitOfHandle<CircuitGroup>(parent);
        CircuitTristate* out = new CircuitTristate(
                wireOfHandle(from, par),
                wireOfHandle(to, par),
                wireOfHandle(enable, par));
        setParent(parent, out);
        return out;
    } catch(const IsomError& e) {
        handleError(e);
        return nullptr;
    }
}

// === Expressions

expr_handle build_expr_const(unsigned val) {
    try {
        return new ExpressionConst(val);
    } catch(const IsomError& e) {
        handleError(e);
        return nullptr;
    }
}

expr_handle build_expr_longconst(const char* value) {
    try {
        try {
            return new ExpressionLongConst(value);
        } catch(const BadHex&) {
            throw IsomError(ISOM_RC_BADHEX);
        }
    } catch(const IsomError& e) {
        handleError(e);
        return nullptr;
    }
}

expr_handle build_expr_var(int input_pin) {
    try {
        return new ExpressionVar(input_pin);
    } catch(const IsomError& e) {
        handleError(e);
        return nullptr;
    }
}

expr_handle build_expr_binop(enum isom_expr_binop op,
        expr_handle left,
        expr_handle right)
{
    try {
        return new ExpressionBinOp(
                exprOfHandle(left),
                exprOfHandle(right),
                (expr::ExpressionBinOperator)op);
    } catch(const IsomError& e) {
        handleError(e);
        return nullptr;
    }
}

expr_handle build_expr_unop(enum isom_expr_unop op, expr_handle expr) {
    try {
        return new ExpressionUnOp(
                exprOfHandle(expr),
                (expr::ExpressionUnOperator)op);
    } catch(const IsomError& e) {
        handleError(e);
        return nullptr;
    }
}

expr_handle build_expr_unop_cst(enum isom_expr_unop_cst op,
        int param,
        expr_handle expr)
{
    try {
        return new ExpressionUnOpCst(
                exprOfHandle(expr),
                param,
                (expr::ExpressionUnOperatorCst)op);
    } catch(const IsomError& e) {
        handleError(e);
        return nullptr;
    }
}

expr_handle build_expr_slice(expr_handle expr, unsigned beg, unsigned end) {
    try {
        return new ExpressionSlice(exprOfHandle(expr), beg, end);
    } catch(const IsomError& e) {
        handleError(e);
        return nullptr;
    }
}

expr_handle build_expr_merge(expr_handle left, expr_handle right) {
    try {
        return new ExpressionMerge(exprOfHandle(left), exprOfHandle(right));
    } catch(const IsomError& e) {
        handleError(e);
        return nullptr;
    }
}

int free_expression(expr_handle expr) {
    try {
        delete exprOfHandle(expr);
        return ISOM_RC_OK;
    } catch(const IsomError& e) {
        return handleError(e);
    }
}

// === Accessors

int isom_input_count(circuit_handle circuit) {
    try {
        CircuitTree* circ = circuitOfHandle(circuit);
        return circ->inputCount();
    } catch(const IsomError& e) {
        handleError(e);
        return -1;
    }
}

int isom_output_count(circuit_handle circuit) {
    try {
        CircuitTree* circ = circuitOfHandle(circuit);
        return circ->outputCount();
    } catch(const IsomError& e) {
        handleError(e);
        return -1;
    }
}

wire_handle isom_nth_input(circuit_handle circuit, size_t wireId) {
    try {
        CircuitTree* circ = circuitOfHandle(circuit);
        if(wireId < circ->inputCount()) {
            WireId* out = circ->nth_input(wireId);
            if(out != nullptr)
                return out->name().c_str();
            lastError = ISOM_RC_NOT_CONNECTED;
        }
        else
            lastError = ISOM_RC_OUT_OF_RANGE;
        return nullptr;
    } catch(const IsomError& e) {
        handleError(e);
        return nullptr;
    }
}

wire_handle isom_nth_output(circuit_handle circuit, size_t wireId) {
    try {
        CircuitTree* circ = circuitOfHandle(circuit);
        if(wireId < circ->outputCount()) {
            WireId* out = circ->nth_output(wireId);
            if(out != nullptr)
                return out->name().c_str();
            lastError = ISOM_RC_NOT_CONNECTED;
        }
        else
            lastError = ISOM_RC_OUT_OF_RANGE;
        return nullptr;
    } catch(const IsomError& e) {
        handleError(e);
        return nullptr;
    }
}

// === Signature

sign_t sign(circuit_handle circuit) {
    try {
        return circuitOfHandle(circuit)->sign();
    } catch(const IsomError& e) {
        handleError(e);
        return 0;
    }
}

sign_t sign_with_precision(circuit_handle circuit, unsigned precision_level) {
    try {
        return circuitOfHandle(circuit)->sign(precision_level);
    } catch(const IsomError& e) {
        handleError(e);
        return 0;
    }
}

// === Circuit matching
match_results* subcircuit_find(circuit_handle needle, circuit_handle haystack){
    try {
        std::vector<MatchResult> res = matchSubcircuit(
                circuitOfHandle<CircuitGroup>(needle),
                circuitOfHandle<CircuitGroup>(haystack));
        match_results* outList = nullptr;

        for(const auto& matchRes: res) {
            match_results* cMatchLink = new match_results;
            memset(cMatchLink, 0x00, sizeof(match_results));
            single_match& cMatch = cMatchLink->match;
            cMatchLink->next = outList;
            outList = cMatchLink;

            for(const auto& part: matchRes.parts) {
                circuit_list* nLink = new circuit_list;
                memset(nLink, 0x00, sizeof(circuit_list));
                nLink->next = cMatch.parts;
                cMatch.parts = nLink;
                nLink->circ = part;
            }
            for(const auto& inWire: matchRes.inputs) {
                wire_list* nLink = new wire_list;
                memset(nLink, 0x00, sizeof(wire_list));
                nLink->next = cMatch.inputs;
                cMatch.inputs = nLink;
                nLink->wire = inWire->name().c_str();
            }
            for(const auto& outWire: matchRes.outputs) {
                wire_list* nLink = new wire_list;
                memset(nLink, 0x00, sizeof(wire_list));
                nLink->next = cMatch.outputs;
                cMatch.outputs = nLink;
                nLink->wire = outWire->name().c_str();
            }
        }
        return outList;
    } catch(const IsomError& e) {
        handleError(e);
        return nullptr;
    }
}

void free_circuit_list(circuit_list* list) {
    while(list != nullptr) {
        circuit_list* toDel = list;
        list = list->next;
        delete toDel;
    }
}

void free_wire_list(wire_list* list) {
    while(list != nullptr) {
        wire_list* toDel = list;
        list = list->next;
        delete toDel;
    }
}

void free_match_results(match_results* res) {
    while(res != nullptr) {
        free_circuit_list(res->match.parts);
        free_wire_list(res->match.inputs);
        free_wire_list(res->match.outputs);
        match_results* toDel = res;
        res = res->next;
        delete toDel;
    }
}

// === Mark & sweep

void isom_clear_marks() {
    markSweepState.marks.clear();
}

void isom_mark_circuit(circuit_handle handle) {
    markSweepState.marks.insert(componentRootOf(circuitOfHandle(handle)));
}

void isom_sweep() {
    auto cMark = markSweepState.marks.begin();
    auto cRoot = markSweepState.roots.begin();
    while(cRoot != markSweepState.roots.end()) {
        if(*cRoot != *cMark)
            cRoot = freeCircuitPtr(*cRoot);
        else {
            ++cRoot;
            ++cMark;
        }
    }

    isom_clear_marks();
}
