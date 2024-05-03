#include "algebra.h"
#include <stdio.h>
#include <math.h>

Matrix create_matrix(int row, int col)
{
    Matrix m;
    m.rows = row;
    m.cols = col;
    return m;
}

Matrix add_matrix(Matrix a, Matrix b)
{
    // ToDo
    if (a.rows == b.rows && a.cols == b.cols)
    {
        Matrix Add = create_matrix(a.rows, a.cols);
        int i, j;
        for (i = 0; i < Add.rows; i++)
        {
            for (j = 0; j < Add.cols; j++)
            {
                Add.data[i][j] = a.data[i][j] + b.data[i][j];
            }
        }
        return Add;
    }
    else
    {
        printf("Error: Matrix a and b must have the same rows and cols.\n");
        return create_matrix(0, 0);
    }
}

Matrix sub_matrix(Matrix a, Matrix b)
{
    // ToDo
    if (a.rows == b.rows && a.cols == b.cols)
    {
        Matrix Sub = create_matrix(a.rows, a.cols);
        int i, j;
        for (i = 0; i < Sub.rows; i++)
        {
            for (j = 0; j < Sub.cols; j++)
            {
                Sub.data[i][j] = a.data[i][j] - b.data[i][j];
            }
        }
        return Sub;
    }
    else
    {
        printf("Error: Matrix a and b must have the same rows and cols.\n");
        return create_matrix(0, 0);
    }
}

Matrix mul_matrix(Matrix a, Matrix b)
{
    // ToDo
    if (a.cols == b.rows)
    {
        Matrix Mul = create_matrix(a.rows, b.cols);
        int i, j, k;
        for (i = 0; i < Mul.rows; i++)
        {
            for (j = 0; j < Mul.cols; j++)
            {
                int sum = 0;
                for (k = 0; k < a.cols; k++)
                {
                    sum += a.data[i][k] + b.data[k][j];
                }
                Mul.data[i][j] = sum;
            }
        }
        return Mul;
    }
    else
    {
        return create_matrix(0, 0);
    }
}

Matrix scale_matrix(Matrix a, double k)
{
    // ToDo

    Matrix Sca = create_matrix(a.rows, a.cols);
    int i, j;
    for (i = 0; i < Sca.rows; i++)
    {
        for (j = 0; j < Sca.cols; j++)
        {
            Sca.data[i][j] = k * a.data[i][j];
        }
    }
    return Sca;
}

Matrix transpose_matrix(Matrix a)
{
    // ToDo
    Matrix Tra = create_matrix(a.cols, a.rows);
    int i, j;
    for (i = 0; i < Tra.rows; i++)
    {
        for (j = 0; j < Tra.cols; j++)
        {
            Tra.data[i][j] = a.data[j][i];
        }
    }
    return Tra;
}

double det_matrix(Matrix a)
{
    // ToDo
    if (a.cols == a.rows)
    {
        double det = 0;
        int i, j, k;
        Matrix Son = create_matrix(a.rows - 1, a.cols - 1);
        // i遍历a的列
        if (a.cols > 2)
        {
            for (i = 0; i < a.cols; i++)
            {
                // j遍历子矩阵的行
                for (j = 0; j < Son.rows; j++)
                { // t遍历子矩阵的列
                    int t = 0;
                    // k遍历a的列
                    for (k = 0; k < a.cols; k++)
                    {
                        // the same col will continue
                        if (k == i)
                        {
                            continue;
                        }
                        else
                        { // 赋值子式
                            Son.data[j][t] = a.data[j + 1][k];
                            t++;
                        }
                    }
                }
                det += (i % 2 ? -1 : 1) * a.data[0][i] * det_matrix(Son);
            }
        }
        else if (a.cols == 2)
        {
            det = a.data[0][0] * a.data[1][1] - a.data[0][1] * a.data[1][0];
        }
        else
        {
            det = a.data[0][0];
        }
        return det;
    }
    else
    {
        printf("Error: The matrix must be a square matrix.\n");
        return 0;
    }
}

Matrix inv_matrix(Matrix a)
{
    // ToDo
    if (det_matrix(a) == 0)
    {
        printf("Error: The matrix is singular.\n");
        return create_matrix(0, 0);
    }
    else
    {
        double det = det_matrix(a);
        // create the A_inv
        Matrix A_inv = create_matrix(a.rows, a.cols);
        // 创建余子式
        Matrix Aji = create_matrix(a.rows - 1, a.cols - 1);
        int i, j, m, n, t = 0, f, g;
        // circulate twice to find an element in matrix a
        for (i = 0; i < a.rows; i++)
        {
            for (j = 0; j < a.cols; j++)
            {
                // define aji by index f and g
                f = g = 0;
                // circulate twice to find all elements in Aij
                for (m = 0; m < a.rows; m++)
                {
                    g = 0;
                    if (m == j)
                    {
                        continue;
                    }
                    else
                    {
                        for (n = 0; n < a.cols; n++)
                        {
                            if (n == i)
                            {
                                continue;
                            }
                            else
                            {
                                Aji.data[f][g++] = a.data[m][n];
                            }
                            f++;
                        }
                    }
                }
                A_inv.data[j][i] = det_matrix(Aji) * ((i + j) % 2 ? -1 : 1) / det;
            }
        }
        return A_inv;
    }
}

int rank_matrix(Matrix a)
{
    // ToDo
    int rank = 0;
    Matrix A = create_matrix(a.rows, a.cols);
    A = a;
    int i, j, k;
    for (i = 0; i < A.rows - 1; i++)
    {
        for (j = i + 1; j < A.rows; j++)
        {
            for (k = 0; k < A.cols; k++)
            {
                if (A.data[j][k] != 0)
                {
                    A.data[j][k] -= A.data[j][k] * A.data[i][0] / A.data[j][0];
                }
                else
                {
                    break;
                }
            }
        }
    }
    for (i = 0; i < A.rows; i++)
    {
        for (j = 0; j < A.cols; j++)
        {
            if (A.data[i][j] != 0)
            {
                rank++;
                break;
            }
        }
    }
    return rank;
}

double trace_matrix(Matrix a)
{
    // ToDo
    if (a.cols == a.rows)
    {
        double trace = 0;
        int i;
        for (i = 0; i < a.cols; i++)
        {
            trace += a.data[i][i];
        }
        return trace;
    }
    else
    {
        printf("Error: The matrix must be a square matrix.\n");
        return 0;
    }
}

void print_matrix(Matrix a)
{
    for (int i = 0; i < a.rows; i++)
    {
        for (int j = 0; j < a.cols; j++)
        {
            // 按行打印，每个元素占8个字符的宽度，小数点后保留2位，左对齐
            printf("%-8.2f", a.data[i][j]);
        }
        printf("\n");
    }
}