#!/bin/bash

Black='\033[0;30m'        # Black
Red='\033[0;31m'          # Red
Green='\033[0;32m'        # Green
Yellow='\033[0;33m'       # Yellow
Blue='\033[0;34m'         # Blue
Purple='\033[0;35m'       # Purple
Cyan='\033[0;36m'         # Cyan
White='\033[0;37m'        # White

BAD="bad/"
GOOD="good/*.cri"
EVALUATOR="${BAD}evaluator/*.cri"
TYPECHECKER="${BAD}typechecker/*.cri"
PARSER="${BAD}parser/*.cri"

PERROR="syntax error"
TERROR="Static Error"
EERROR="Runtime Error"

binary=./interpreter 

passed=0
counter=0

printf "${Cyan}Testing Cringe Interpreter${White}\n"
printf "${Purple}Testing Bad examples${White}\n"
printf "${Blue}Testing PARSER${White}\n"

for file in $PARSER
do
    printf "${Yellow} [TESTING ${file}]... ${Cyan}"
    if ${binary} ${file}; then
        printf "${Red} No errors detected where they should"
    else 
        printf "${Green} ========================================PASSED========================================"
        let "passed+=1"
    fi
    printf "${White}\n"
    let "counter+=1"      
done

printf "${Blue}Testing TYPECHECKER${White}\n"

for file in $TYPECHECKER
do
    printf "${Yellow} [TESTING ${file}]... ${Cyan}"
    if ${binary} ${file}; then
        printf "${Red} No errors detected where they should"
    else 
        printf "${Green} ========================================PASSED========================================"
        let "passed+=1"
    fi
    printf "${White}\n"
    let "counter+=1"        
done

printf "${Blue}Testing EVALUATOR${White}\n"

for file in $EVALUATOR
do
    printf "${Yellow} [TESTING ${file}]... ${Cyan}"
    if ${binary} ${file}; then
        printf "${Red} No errors detected where they should"
    else 
        printf "${Green} ========================================PASSED========================================"
        let "passed+=1"
    fi
    printf "${White}\n"
    let "counter+=1"        
done

printf "\n${Purple}Testing Good examples${White}\n"

for file in $GOOD
do
    printf "${Yellow} [TESTING ${file}]... ${Cyan}"
    if ${binary} ${file}; then
        printf "${Green} ========================================PASSED========================================"
        let "passed+=1"
    else 
        printf "${Red} ERROR..."
    fi
    printf "${White}\n"
    let "counter+=1"     
done

printf "${Green}PASSED ${passed}/${counter} TESTS.${White}\n"
