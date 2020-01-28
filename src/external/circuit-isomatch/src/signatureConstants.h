/** Constants used to compute signatures. */

#pragma once

#include <cstdint>

/// Type of a gate/expression signature
typedef uint64_t sign_t;

namespace signatureConstants {
    /**
     * Holds constants values and an evaluation function to sign a given hash
     * with those values.
     */
    struct OperConstants {
        OperConstants(uint32_t add, uint32_t lmul, uint32_t hmul,
                uint32_t lmod, uint32_t hmod) :
            add(add), lowMul(lmul), highMul(hmul),
            lowMod(lmod), highMod(hmod) {}

        uint32_t add;
        uint32_t lowMul, highMul;
        uint32_t lowMod, highMod;

        /// Signs the hash
        uint64_t operator() (uint64_t v) const;
    };

    // ==== CONSTANTS ==== (Auto-generated)
    // ===================
    const uint32_t pinIdMod = 895948033;
    const uint32_t sliceMulInner = 166597;
    const OperConstants opcst_and(3390840479u, 659787649u, 165943601u, 913324081u, 153863837u);
    const OperConstants opcst_or(1325711570u, 546652657u, 578143253u, 848168701u, 166296997u);
    const OperConstants opcst_xor(897089714u, 456534161u, 899919529u, 139709989u, 370779613u);
    const OperConstants opcst_add(2594856353u, 381677137u, 826551697u, 577717813u, 875240477u);
    const OperConstants opcst_sub(1591210389u, 181017349u, 561251857u, 588545437u, 728454121u);
    const OperConstants opcst_mul(1606209404u, 467039017u, 522311969u, 849701813u, 498988001u);
    const OperConstants opcst_div(1906429065u, 797837389u, 692812177u, 416197277u, 207271217u);
    const OperConstants opcst_mod(922690042u, 457746049u, 871434749u, 626813029u, 899778277u);
    const OperConstants opcst_lsl(177262002u, 407128973u, 162284581u, 148542397u, 180592501u);
    const OperConstants opcst_lsr(1099106606u, 467962637u, 279812881u, 304196353u, 985148513u);
    const OperConstants opcst_asr(985559327u, 707665493u, 229779541u, 161172929u, 435856549u);
    const OperConstants opcst_not(4233971181u, 316668013u, 753715793u, 322126369u, 878028017u);
    const OperConstants opcst_un_lsr(1851723828u, 875770529u, 591999293u, 915715777u, 823881029u);
    const OperConstants opcst_un_lsl(933223001u, 124040209u, 706505141u, 636431837u, 277563037u);
    const OperConstants opcst_un_asr(3544854494u, 874482337u, 588529969u, 631913089u, 597600221u);
    const OperConstants opcst_cstint(3831503986u, 304930261u, 543302069u, 739263797u, 958652273u);
    const OperConstants opcst_wireid(2472436353u, 895959769u, 105554441u, 259819121u, 438174169u);
    const OperConstants opcst_numconst(1399781387u, 598999393u, 212481253u, 672017761u, 748863581u);
    const OperConstants opcst_longconst(3376341841u, 858559253u, 669810637u, 653202269u, 653181341u);
    const OperConstants opcst_merge(2616661224u, 775975373u, 747472573u, 708846049u, 741751853u);
    const OperConstants opcst_slice(824635605u, 654813793u, 853789501u, 286822961u, 827230109u);
    const OperConstants opcst_slicebounds(70524680u, 444607909u, 848056189u, 509462321u, 700093841u);
    const OperConstants opcst_leaftype(3087564275u, 301977869u, 635134589u, 360062929u, 650573921u);
    const OperConstants opcst_groupIO(2005644964u, 859388701u, 536741141u, 593820389u, 584754689u);

}
