// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.

#ifndef ANALYSISPROPS_H
#define ANALYSISPROPS_H

namespace llvm {

    template<bool IsMayAnalysis, bool IsInterprocedural,
             bool FlowSensitivity, bool ContextSensitivity,
             bool FieldSensitivity, bool ControlSensitivity>
    struct AnalysisProperties {
        static bool const isMayAnalysis = IsMayAnalysis;
        static bool const isMustAnalysis = !isMayAnalysis;
        static bool const isInterprocedural = IsInterprocedural;
        static bool const isIntraprocedural = !isInterprocedural;
        static bool const flowSensitivity = FlowSensitivity;
        static bool const contextSensitivity = ContextSensitivity;
        static bool const fieldSensitivity = FieldSensitivity;
        static bool const controlSensitivity = ControlSensitivity;
    };

    template<typename Algorithm>
    struct AlgorithmProperties;

}

#endif
