#include <ctype.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdbool.h>
#include <ta-lib/ta_func.h>
#include <ta-lib/ta_abstract.h>

typedef enum {DOUBLE, INT, DOUBLEARRAY, INTARRAY} datatype;

typedef struct HS_Params {
   /* Params we need to generate Haskell code */
                                // e.g.
   char funcName[200];          // "Aroon"
   char funcNameUpper[200];     // "AROON"
   char funcNameLower[200];     // "aroon"

   char inputNames[20][200];    // ["inHigh", "inLow"]
   int inputs;                  // 2

   char optInputNames[20][200]; // ["optInTimePeriod"]
   datatype optInputTypes[20];  // [INT]
   int optInputs;               // 1

   char outputNames[20][200];   // ["outAroonDown", "outAroonUp"]
   datatype outputTypes[20];   // [DOUBLEARRAY, DOUBLEARRAY]
   int outputs;                 // 2

   // true if all return vals are double[]
   bool returnsDoubles;        // true
} HS_Params;

int popcount(int i) {
    return __builtin_popcount(i);
}

HS_Params getHsParams(const TA_FuncInfo *taFuncInfo) {
    HS_Params params;
    const char *name = taFuncInfo->name;
    int nbInput = taFuncInfo->nbInput;
    const TA_FuncHandle *handle = taFuncInfo->handle;

    int nameLen = strlen(name);
    for (int i = 0; i < nameLen; i++) {
        params.funcNameUpper[i] = toupper(name[i]);
        params.funcNameLower[i] = tolower(name[i]);
    }
    params.funcNameUpper[nameLen] = '\0';
    params.funcNameLower[nameLen] = '\0';

    params.inputs = 0;

    for (int i = 0; i < nbInput; i++) {
        TA_InputParameterInfo inputParam;
        const TA_InputParameterInfo *pointer = &inputParam;
        TA_GetInputParameterInfo(handle, i, &pointer);
        TA_InputFlags flags = pointer->flags;
        if (!flags) {
            params.inputs++;
        } else {
            params.inputs += popcount(flags);
        }
    }

    int curInput = 0;
    for (int i = 0; i < nbInput; i++) {
        TA_InputParameterInfo inputParam;
        const TA_InputParameterInfo *pointer = &inputParam;
        TA_GetInputParameterInfo(handle, i, &pointer);

        const char *paramName = pointer->paramName;

        // DAS: as far as I can tell, these are always double[]
        // TA_InputParameterType
        // 0 = TA_Input_Price
        // 1 = TA_Input_Real
        // 2 = TA_Input_Integer  (never used)

        TA_InputFlags flags = pointer->flags;

        if (!flags) {
            strcpy(params.inputNames[curInput], paramName);
            curInput++;
        }

        // typedef int TA_InputFlags;
        // #define TA_IN_PRICE_OPEN         0x00000001
        // #define TA_IN_PRICE_HIGH         0x00000002
        // #define TA_IN_PRICE_LOW          0x00000004
        // #define TA_IN_PRICE_CLOSE        0x00000008
        // #define TA_IN_PRICE_VOLUME       0x00000010
        // #define TA_IN_PRICE_OPENINTEREST 0x00000020
        // #define TA_IN_PRICE_TIMESTAMP    0x00000040
        if (flags & TA_IN_PRICE_OPEN) {
            strcpy(params.inputNames[curInput], "inOpen");
            if (nbInput > 1)
                sprintf(params.inputNames[curInput]+6, "%d", i);
            curInput++;
        }

        if (flags & TA_IN_PRICE_HIGH) {
            strcpy(params.inputNames[curInput], "inHigh");
            if (nbInput > 1)
                sprintf(params.inputNames[curInput]+6, "%d", i);
            curInput++;
        }

        if (flags & TA_IN_PRICE_LOW) {
            strcpy(params.inputNames[curInput], "inLow");
            if (nbInput > 1)
                sprintf(params.inputNames[curInput]+5, "%d", i);
            curInput++;
        }

        if (flags & TA_IN_PRICE_CLOSE) {
            strcpy(params.inputNames[curInput], "inClose");
            if (nbInput > 1)
                sprintf(params.inputNames[curInput]+7, "%d", i);
            curInput++;
        }

        if (flags & TA_IN_PRICE_VOLUME) {
            strcpy(params.inputNames[curInput], "inVolume");
            if (nbInput > 1)
                sprintf(params.inputNames[curInput]+8, "%d", i);
            curInput++;
        }

        if (flags & TA_IN_PRICE_OPENINTEREST) {
            strcpy(params.inputNames[curInput], "inOpenInterest");
            if (nbInput > 1)
                sprintf(params.inputNames[curInput]+14, "%d", i);
            curInput++;
        }

        if (flags & TA_IN_PRICE_TIMESTAMP) {
            strcpy(params.inputNames[curInput], "inTimestamp");
            if (nbInput > 1)
                sprintf(params.inputNames[curInput]+11, "%d", i);
            curInput++;
        }
    }

    params.optInputs = taFuncInfo->nbOptInput;
    for (int i = 0; i < params.optInputs; i++) {
        TA_OptInputParameterInfo optInputParam;
        const TA_OptInputParameterInfo *pointer = &optInputParam;
        TA_GetOptInputParameterInfo(handle, i, &pointer);

        const char *paramName = pointer->paramName;
        strcpy(params.optInputNames[i], paramName);

        // TA_OptInputParameterType
        // 0 = TA_OptInput_RealRange
        // 1 = TA_OptInput_RealList
        // 2 = TA_OptInput_IntegerRange
        // 3 = TA_OptInput_IntegerList

        TA_OptInputParameterType type = pointer->type;
        if (type==0 || type==1) {
            params.optInputTypes[i] = DOUBLE;
        } else {
            params.optInputTypes[i] = INT;
        }
    }

    params.outputs = taFuncInfo->nbOutput;
    params.returnsDoubles = true;
    for (int i = 0; i < params.outputs; i++) {
        TA_OutputParameterInfo outputParam;
        const TA_OutputParameterInfo *pointer = &outputParam;
        TA_GetOutputParameterInfo(handle, i, &pointer);

        const char *outputName = pointer->paramName;
        strcpy(params.outputNames[i], outputName);

        // TA_OutputParameterType
        // 0 = TA_Output_Real
        // 1 = TA_Output_Integer

        TA_OutputParameterType type = pointer->type;

        if (type > 0) {
            params.returnsDoubles = false;
            params.outputTypes[i] = DOUBLEARRAY;
        } else {
            params.outputTypes[i] = INTARRAY;
        }
    }

    return params;
}

void terpri() {
    printf("\n");
}

void printHsParam(HS_Params hsParams) {
    printf("%s\n", hsParams.funcNameLower);
    printf("%s\n", hsParams.funcNameUpper);
    terpri();

    for (int i = 0; i < hsParams.inputs; i++) {
        printf("input %d\n", i);
        printf("  %s\n", hsParams.inputNames[i]);
    }

    for (int i = 0; i < hsParams.optInputs; i++) {
        printf("optInput %d\n", i);
        printf("  %s\n", hsParams.optInputNames[i]);
        printf("  %d\n", hsParams.optInputTypes[i]);
    }

    for (int i = 0; i < hsParams.outputs; i++) {
        printf("output %d\n", i);
        printf("  %s\n", hsParams.outputNames[i]);
        printf("  %d\n", hsParams.outputTypes[i]);
    }
}

void forEach(const TA_FuncInfo *funcInfo, void *opaqueData) {
    const char *name = funcInfo->name; // e.g., AROON

    // e.g., -- AVGPRICE             Average Price
    printf("-- %-20s %s\n", name, funcInfo->hint);
    terpri();

    HS_Params hsParams = getHsParams(funcInfo);
    //printHsParam(hsParams);

    // currently we only support ta-lib functions that return one or more double[],
    // not int[].

    if (!hsParams.returnsDoubles) {
        printf("-- Currently Unsupported\n");
        terpri();
        return;
    }

    char args[2000];
    args[0] = '\0';
    for (int i = 0; i < hsParams.inputs; i++) {
        if (i > 0)
            strcat(args, " ");
        strcat(args, hsParams.inputNames[i]);
    }

    for (int i = 0; i < hsParams.optInputs; i++) {
        strcat(args, " ");
        strcat(args, hsParams.optInputNames[i]);
    }

    char optTypesMerged[200];
    if (!hsParams.optInputs) {
        strcpy(optTypesMerged, "_");
    } else {
        strcpy(optTypesMerged, "\0");
        for (int i = 0; i < hsParams.optInputs; i++) {
            char* typeString;
            if (hsParams.optInputTypes[i] == INT)
                typeString = "Int";
            else
                typeString = "Double";
            strcat(optTypesMerged, typeString);
        }
    }

    char ctaSig[200] = "";            // "CTA2Int2"
    char tsSig[200] = "";             // "TS2Int"
    char taSig[200] = "";             // "TA2Int2"


    /*

    type CTA1IntInt1 = CInt        -- startIdx
                -> CInt        -- endIdx
                -> Ptr CDouble -- input array
                -> CInt        -- option
                -> CInt        -- option
                -> Ptr CInt    -- outBegIdx
                -> Ptr CInt    -- outNBElement
                -> Ptr CDouble -- output array
                -> IO CInt

     */

    strcat(ctaSig, "CInt -> CInt -> ");

    for (int i = 0; i < hsParams.inputs; i++) {
        if (i > 0) {
            strcat(tsSig, " -> ");
            strcat(ctaSig, " -> ");
        }
        strcat(tsSig, "[Double]");
        strcat(ctaSig, "Ptr CDouble");
    }

    for (int i = 0; i < hsParams.optInputs; i++) {
        strcat(tsSig, " -> ");
        strcat(ctaSig, " -> ");

        char *hsOptType = hsParams.optInputTypes[i] == INT ? "Int" : "Double";
        strcat(tsSig, hsOptType);

        char *cHsOptType = hsParams.optInputTypes[i] == INT ? "CInt" : "CDouble";
        strcat(ctaSig, cHsOptType);
    }

    strcat(ctaSig, " -> Ptr CInt -> Ptr CInt");
    for (int i = 0; i < hsParams.outputs; i++) {
        strcat(ctaSig, " -> ");
        char *cHsOutputType = hsParams.outputTypes[i] == INTARRAY ? "Ptr CInt" : "Ptr CDouble";
        strcat(ctaSig, cHsOutputType);
    }

    strcat(tsSig, " -> IO (Either Int TaOutput)");
    strcat(ctaSig, " -> IO CInt");

    sprintf(taSig, "TA%d%s%d", hsParams.inputs, optTypesMerged, hsParams.outputs);

    printf("foreign import ccall unsafe \"ta_func.h TA_%s\"\n", hsParams.funcNameUpper);
    printf("  c_ta_%s :: %s\n", hsParams.funcNameLower, ctaSig);

    terpri();

    printf("ta_%s :: %s\n", hsParams.funcNameLower, tsSig);
    printf("ta_%s %s\n", hsParams.funcNameLower, args);
    printf("    = withArray _inReal            $ \\cInReal ->\n");
    printf("      alloca                       $ \\cOutBegIdx ->\n");
    printf("      alloca                       $ \\cOutNbElement ->\n");
    printf("      allocaArray (len * outputs)  $ \\cOutReal ->\n");
    printf("      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)\n");
    printf("          getInArrPtr i   = getArrPtr i cInReal\n");
    printf("          getOutArrPtr i  = getArrPtr i cOutReal\n");
    printf("      in do\n");
//        -- TODO: this stuff has to be auto-generated (getInArrPtr 1) (getInArrPtr 2), ... (fromIntegral arg2) (from... arg3)
//        -- c function name also has to be auto-generated
//        rc <- c_ta_rsi startIdx endIdx (getInArrPtr 0) (fromIntegral optInTimePeriod) cOutBegIdx cOutNbElement (getOutArrPtr 0)

    printf("        rc <- c_ta_%s startIdx endIdx", hsParams.funcNameLower);
    for (int i = 0; i < hsParams.inputs; i++) {
        printf(" (getInArrPtr %d)", i);
    }
    for (int i = 0; i < hsParams.optInputs; i++) {
        char *fn = (hsParams.optInputTypes[i] == INT) ? "fromIntegral" : "realToFrac";
        printf(" (%s %s)", fn, hsParams.optInputNames[i]);
    }
    printf(" cOutBegIdx cOutNbElement");
    for (int i = 0; i < hsParams.outputs; i++) {
        printf(" (getOutArrPtr %d)", i);
    }
    printf("\n");

    printf("        case rc of\n");
    printf("          0 -> do\n");
    printf("               outReal <- peekArray (len * outputs) cOutReal\n");
    printf("               outBegIdx <- peek cOutBegIdx\n");
    printf("               outNbElement <- peek cOutNbElement\n");
    printf("               return $ Right $ TaOutput { outBegIdx = fromIntegral outBegIdx,\n");
    printf("                                           outNBElement = fromIntegral outNbElement,\n");
    printf("                                           outCount = outputs,\n");
    printf("                                           out = chunksOf len outReal\n");
    printf("                                         }\n");
    printf("          _ -> return $ Left $ fromIntegral rc\n");
//    where _inReal = inReal -- TODO: inReal should be auto-gen

    printf("    where _inReal = %s", hsParams.inputNames[0]);
    for (int i = 1; i < hsParams.inputs; i++) {
        printf(" ++ %s", hsParams.inputNames[i]);
    }
    printf("\n");

    printf("          len = fromIntegral $ length %s\n", hsParams.inputNames[0]);
    printf("          startIdx = 0\n");
    printf("          endIdx = fromIntegral $ len - 1\n");
    printf("          outputs = %d\n", hsParams.outputs);

    terpri();

}

int main() {
    TA_Initialize();

    TA_ForEachFunc(forEach, NULL);

    return EXIT_SUCCESS;
}

