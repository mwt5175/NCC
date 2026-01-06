# NCC

## Neptune Software Suite

The Neptune Software Suite is a research project I started along time ago. This is still hosted on BitBucket, moving to GitHub so all projects are in one location. This includes NCC1 which is a rewrite and most up-to-date codebase of NCC. NCC was part of the Neptune OS codebase however has since been moved to its own repository due to its complexity. The Neptune OS, NCC, and related SDK's and other tools are a Work In Progress (WIP), run at your own risk.

## Introduction

The Neptune C Compiler targets Neptune Assembler (NASM) Intel style syntax. NASM targets Win32 COFF object files compatible with Visual Studio's LINK. NASM source code is hosted as part of the Neptune OS source tree.

## Future plans

NCC is not completed; it supports most of the C syntax and generates correct NASM code, however options are being considered for integrating symbolic information between NCC, NASM, and NDBG for use by NBOOT, NEXEC, and user-mode applications. NASM has some support for SLAB's however NCC does not generate this at this time.

NCC also lacks floating point calls and basic runtime library calls and supporting functions.
