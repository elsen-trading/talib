#include <ctype.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdbool.h>
#include <stdarg.h>
#include <ta-lib/ta_func.h>
#include <ta-lib/ta_abstract.h>

typedef enum {DOUBLE, INT, DOUBLEARRAY, INTARRAY} datatype;

void printfIndent(int indent, const char *fmt, ...) {
    va_list args;
    va_start(args, fmt);
    printf("%*s", indent, " ");
    vprintf(fmt, args);
    va_end(args);
}

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
            params.outputTypes[i] = INTARRAY;
        } else {
            params.outputTypes[i] = DOUBLEARRAY;
        }
    }

    return params;
}

void terpri() {
    printf("\n");
}

void printHsParam(const TA_FuncInfo *funcInfo, void *opaqueData) {
    HS_Params hsParams = getHsParams(funcInfo);
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

void printHsCode(const TA_FuncInfo *funcInfo, void *opaqueData) {
    HS_Params hsParams = getHsParams(funcInfo);
    const char *name = funcInfo->name; // e.g., AROON
    printf("--\n");
    // e.g., -- AVGPRICE             Average Price
    printf("-- %-20s %s\n", name, funcInfo->hint);
    printf("--\n");
    terpri();

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

    char ctaSig[200] = "";
    char tsSig[200] = "";

    strcat(ctaSig, "CInt -> CInt -> ");

    for (int i = 0; i < hsParams.inputs; i++) {
        if (i > 0) {
            strcat(tsSig, " -> ");
            strcat(ctaSig, " -> ");
        }
        strcat(tsSig, "V.Vector Double");
        strcat(ctaSig, "Ptr Double");
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
        char *cHsOutputType = hsParams.outputTypes[i] == INTARRAY ? "Ptr Int" : "Ptr Double";
        strcat(ctaSig, cHsOutputType);
    }

    strcat(tsSig, " -> IO (Either Int (Int, Int");
    for (int i = 0; i < hsParams.outputs; i++) {
        strcat(tsSig, hsParams.outputTypes[i] == INTARRAY ? ", V.Vector Int" : ", V.Vector Double");
    }
    strcat(tsSig, "))");

    strcat(ctaSig, " -> IO CInt");

    printf("foreign import ccall unsafe \"ta_func.h TA_%s\"\n", hsParams.funcNameUpper);
    printf("  c_ta_%s :: %s\n", hsParams.funcNameLower, ctaSig);

    terpri();
    printf("-- inputs");
    for (int i = 0; i < hsParams.inputs; i++) {
        printf("\n--   %s", hsParams.inputNames[i]);
    }
    printf("\n");
    printf("-- arguments");
    for (int i = 0; i < hsParams.optInputs; i++) {
        printf("\n--   %s (%s)", hsParams.optInputNames[i], hsParams.optInputTypes[i] == INT ? "int" : "double");
    }
    printf("\n");
    printf("-- outputs");
    for (int i = 0; i < hsParams.outputs; i++) {
        printf("\n--   %s (%s)", hsParams.outputNames[i], hsParams.outputTypes[i] == INTARRAY ? "int[]" : "double[]");
    }
    printf("\n");
    terpri();

    // All functions that have multiple returns have the same type (double[] or int[]) for all return arrays.
    // The following return multiple double[]s (number in parens = number of outputs):
    // AROON (2),
    // BBANDS (3),
    // HT_PHASOR (2),
    // HT_SINE (2),
    // MACD (3),
    // MACDEXT (3),
    // MACDFIX (3),
    // MAMA (2),
    // MINMAX (2),
    // STOCH (2),
    // STOCHF (2),
    // STOCHRSI (2)
    //
    // The following return multiple int[]s:
    // MINMAXINDEX (2)

    printf("ta_%s :: %s\n", hsParams.funcNameLower, tsSig);
    printf("ta_%s %s = do\n", hsParams.funcNameLower, args);

    int indent = 4;
    for (int i = 0; i < hsParams.inputs; i++) {
        char *in = hsParams.inputNames[i];
        printfIndent(indent, "_%s <- V.thaw %s\n", in, in);
    }

    for (int i = 0; i < hsParams.outputs; i++) {
        printfIndent(indent, "_%s <- VM.new len\n", hsParams.outputNames[i]);
    }

    for (int i = 0; i < hsParams.inputs; i++) {
        char *in = hsParams.inputNames[i];
        printfIndent(indent, "withForeignPtr (vecPtr _%s) $ \\c_%s ->\n", in, in);
        indent += 2;
    }

    printfIndent(indent, "alloca $ \\cOutBegIdx ->\n");
    indent += 2;
    printfIndent(indent, "alloca $ \\cOutNbElement ->\n");
    indent += 2;

    for (int i = 0; i < hsParams.outputs; i++) {
        char *out = hsParams.outputNames[i];
        printfIndent(indent, "withForeignPtr (vecPtr _%s) $ \\c_%s ->\n", out, out);
        indent += 2;
    }

    printfIndent(indent, "do rc <- c_ta_%s 0 (fromIntegral $ len - 1)", hsParams.funcNameLower);
    for (int i = 0; i < hsParams.inputs; i++) {
        printf(" c_%s", hsParams.inputNames[i]);
    }

    for (int i = 0; i < hsParams.optInputs; i++) {
        char *fn = (hsParams.optInputTypes[i] == INT) ? "fromIntegral" : "realToFrac";
        printf(" (%s %s)", fn, hsParams.optInputNames[i]);
    }

    printf(" cOutBegIdx cOutNbElement");
    for (int i = 0; i < hsParams.outputs; i++) {
        printf(" c_%s", hsParams.outputNames[i]);
    }
    printf("\n");

    indent += 3;

    for (int i = 0; i < hsParams.outputs; i++) {
        char *out = hsParams.outputNames[i];
        printfIndent(indent, "out_%s <- V.freeze _%s\n", out, out);
    }

    printfIndent(indent, "case rc of\n");
    indent += 2;
    int caseIndent = indent;
    printfIndent(indent, "0 -> do outBegIdx <- peek cOutBegIdx\n");
    indent += 8;
    printfIndent(indent, "outNbElement <- peek cOutNbElement\n");
    printfIndent(indent, "return $ Right $ (fromIntegral outBegIdx,\n");
    indent += 18;
    printfIndent(indent, "fromIntegral outNbElement");
    for (int i = 0; i < hsParams.outputs; i++) {
        printf(",\n");
        printfIndent(indent, "out_%s", hsParams.outputNames[i]);
    }
    printf(")\n");
    indent = caseIndent;
    printfIndent(indent, "_ -> return $ Left $ fromIntegral rc\n");
    indent = 2;
    printfIndent(indent, "where\n");
    indent = 4;
    printfIndent(indent, "len = fromIntegral $ V.length %s\n", hsParams.inputNames[0]);
    terpri();
}

// a check to see if functions that return multiple outputs ever return outputs
// of varying type. That is never the case. If a function returns one int[], it returns all int[]s.
// if a fuction returns one double[], it returns all double[]s
void printOutputTypes(const TA_FuncInfo *funcInfo, void *opaqueData) {
    HS_Params hsParams = getHsParams(funcInfo);
    const char *name = funcInfo->name; // e.g., AROON
    if (hsParams.outputs <= 1)
        return;
    printf("-- %-20s %s\n", name, funcInfo->hint);
    for (int i = 0; i < hsParams.outputs; i++) {
        printf("%s - %d\n", hsParams.outputNames[i], hsParams.outputTypes[i]);
    }
    terpri();
}

int main() {
    TA_Initialize();

    TA_ForEachFunc(printHsCode, NULL);

    return EXIT_SUCCESS;
}

