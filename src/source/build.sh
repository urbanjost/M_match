#!/bin/bash
cd $(dirname $0)
rm -rfv ../source/doc
export GITHUB=TRUE
export DEMO_OUTDIR=../../example/
export DEMO_SUBDIR=FALSE
GPF_build_module M_match
exit
