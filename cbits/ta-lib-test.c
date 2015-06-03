// gcc ta-lib-test.c -lta_lib

#include <stdio.h>
#include <stdlib.h>
#include <ta-lib/ta_func.h>
#include <ta-lib/ta_abstract.h>

void printArray(double arr[], int size) {
    printf("[");
    for (int i = 0; i < size; i++) {
        if (i > 0)
            printf(", ");
        printf("%f", arr[i]);
    }
    printf("]");
}

void printIntArray(int arr[], int size) {
    printf("[");
    for (int i = 0; i < size; i++) {
        if (i > 0)
            printf(", ");
        printf("%d", arr[i]);
    }
    printf("]");
}

void printBegNb(int outBegIdx, int outNbElement) {
    printf("outBegIdx: %d\n", outBegIdx);
    printf("outNbElement: %d\n", outNbElement);
}

void initArrayVal(double arr[], int size, double val) {
    for (int i = 0; i < size; i++) {
        arr[i] = val;
    }
}

void initArray(double arr[], int size) {
    initArrayVal(arr, size, -1);
}

void initIntArrayVal(int arr[], int size, int val) {
    for (int i = 0; i < size; i++) {
        arr[i] = val;
    }
}

void initIntArray(int arr[], int size) {
    initIntArrayVal(arr, size, -1);
}

int main() {
    TA_Initialize();

    // Apple stock prices. April 1, 2015 through April 30, 2015.

    double open[] = {124.82, 125.03, 124.47, 127.64, 125.85,
                     125.85, 125.95, 128.37, 127.00, 126.41,
                     126.28, 125.55, 125.57, 128.10, 126.99,
                     128.30, 130.49, 132.31, 134.46, 130.16,
                     128.64};

    double high[] = {125.12, 125.56, 127.51, 128.12, 126.40,
                     126.58, 127.21, 128.57, 127.29, 127.13,
                     127.10, 126.14, 128.12, 128.20, 128.87,
                     130.42, 130.63, 133.13, 134.54, 131.59,
                     128.64};

    double low[] = {123.10, 124.19, 124.33, 125.98, 124.97,
                    124.66, 125.26, 126.61, 125.91, 126.01,
                    126.11, 124.46, 125.17, 126.67, 126.32,
                    128.14, 129.23, 131.15, 129.57, 128.30,
                    124.58};

    double close[] = {124.25, 125.32, 127.35, 126.01, 125.60,
                     126.56, 127.10, 126.85, 126.30, 126.78,
                     126.17, 124.75, 127.60, 126.91, 128.62,
                     129.67, 130.28, 132.65, 130.56, 128.64,
                     125.15};

    double volume[] = {40621400, 32220100, 37194000, 35012300, 37329200,
                       32484000, 40188000, 36365100, 25524600, 28970400,
                       28369000, 51957000, 47054300, 32435100, 37654500,
                       45770900, 44525900, 96954200, 118924000, 63386100,
                       83195400};

    double adjclose[] = {123.73, 124.80, 126.82, 125.49, 125.08,
                         126.03, 126.57, 126.32, 125.77, 126.25,
                         125.65, 124.23, 127.07, 126.38, 128.08,
                         129.13, 129.74, 132.10, 130.02, 128.10,
                         124.63};

    int len = sizeof(open)/sizeof(open[0]);

    int startIdx = 0;
    int endIdx = len-1;

    double outReal1[len];
    double outReal2[len];
    int outInt1[len];
    for (int i = 0; i < len; i++) {
        outInt1[i] = 3;
    }

    printIntArray(outInt1, len);
    printf("\n");

    initArray(outReal1, len);
    initArray(outReal2, len);
    initIntArray(outInt1, len);

    printf("close\n\n");
    printArray(close, len);
    printf("\n\n");

    TA_Integer outBeg;
    TA_Integer outNbElement;

    printf("Chaikin A/D Line\n");
    initArray(outReal1, len);
    TA_AD(startIdx, endIdx, high, low, close, volume, &outBeg, &outNbElement, outReal1);
    printBegNb(outBeg, outNbElement);
    printArray(outReal1, len);
    printf("\n\n");

    printf("Chaikin A/D Oscillator\n");
    initArray(outReal1, len);
    TA_ADOSC(startIdx, endIdx, high, low, close, volume, 6, 7, &outBeg, &outNbElement, outReal1);
    printBegNb(outBeg, outNbElement);
    printArray(outReal1, len);
    printf("\n\n");

    printf("Average Directional Movement Index\n");
    initArray(outReal1, len);
    TA_ADX(startIdx, endIdx, high, low, close, 6, &outBeg, &outNbElement, outReal1);
    printBegNb(outBeg, outNbElement);
    printArray(outReal1, len);
    printf("\n\n");

    printf("Aroon\n");
    initArray(outReal1, len);
    initArray(outReal2, len);
    TA_AROON(startIdx, endIdx, high, low, 5, &outBeg, &outNbElement, outReal1, outReal2);
    printBegNb(outBeg, outNbElement);
    printArray(outReal1, len);
    printf("\n");
    printArray(outReal2, len);
    printf("\n\n");

    printf("Average True Range\n");
    initArray(outReal1, len);
    TA_ATR(startIdx, endIdx, high, low, close, 14, &outBeg, &outNbElement, outReal1);
    printBegNb(outBeg, outNbElement);
    printArray(outReal1, len);
    printf("\n\n");

    printf("Moving Average\n");
    initArray(outReal1, len);
    TA_MA(startIdx, endIdx, close, 5, TA_INTEGER_DEFAULT, &outBeg, &outNbElement, outReal1);
    printBegNb(outBeg, outNbElement);
    printArray(outReal1, len);
    printf("\n\n");

    printf("High-Wave Candle\n");
    initIntArray(outInt1, len);
    TA_CDLHIGHWAVE(startIdx, endIdx, open, high, low, close, &outBeg, &outNbElement, outInt1);
    printBegNb(outBeg, outNbElement);
    printIntArray(outInt1, len);
    printf("\n\n");

    printf("Hanging Man\n");
    initIntArray(outInt1, len);
    TA_CDLHANGINGMAN(startIdx, endIdx, open, high, low, close, &outBeg, &outNbElement, outInt1);
    printBegNb(outBeg, outNbElement);
    printIntArray(outInt1, len);
    printf("\n\n");

    printf("Media Price\n");
    initArray(outReal1, len);
    TA_MEDPRICE(startIdx, endIdx, high, low, &outBeg, &outNbElement, outReal1);
    printBegNb(outBeg, outNbElement);
    printArray(outReal1, len);
    printf("\n\n");

    printf("Money Flow Index\n");
    initArray(outReal1, len);
    TA_MFI(startIdx, endIdx, high, low, close, volume, 7, &outBeg, &outNbElement, outReal1);
    printBegNb(outBeg, outNbElement);
    printArray(outReal1, len);
    printf("\n\n");

    printf("Relative Strength Index\n");
    initArray(outReal1, len);
    TA_RSI(startIdx, endIdx, close, 9, &outBeg, &outNbElement, outReal1);
    printBegNb(outBeg, outNbElement);
    printArray(outReal1, len);
    printf("\n\n");

    printf("Simple Moving Average\n");
    initArray(outReal1, len);
    TA_SMA(startIdx, endIdx, close, 8, &outBeg, &outNbElement, outReal1);
    printBegNb(outBeg, outNbElement);
    printArray(outReal1, len);
    printf("\n\n");

    return EXIT_SUCCESS;
}


