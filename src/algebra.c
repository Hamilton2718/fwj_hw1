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
                double sum = 0;
                for (k = 0; k < a.cols; k++)
                {
                    sum += a.data[i][k] * b.data[k][j];
                }
                if (sum > -0.005 && sum <= 0.000)
                {
                    sum = 0.00;
                }
                Mul.data[i][j] = sum;
            }
        }
        return Mul;
    }
    else
    {
        printf("Error: The number of cols of matrix a must be equal to the number of rows of matrix b.\n");
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
        // 创建A逆
        Matrix A_inv = create_matrix(a.rows, a.cols);
        // 创建a[i][j]的余子式
        Matrix Aji = create_matrix(a.rows - 1, a.cols - 1);
        int i, j, m, n, f, g;
        // 二重循环遍历a中元素
        for (i = 0; i < a.rows; i++)
        {
            for (j = 0; j < a.cols; j++)
            {
                // f和g作为Aji的索引
                f = g = 0;
                // 找出划掉i行j列的余子式的所有元素
                for (m = 0; m < a.rows; m++)
                {
                    g = 0;
                    if (m == i)
                    {
                        continue;
                    }
                    else
                    {
                        for (n = 0; n < a.cols; n++)
                        {
                            if (n == j)
                            {
                                continue;
                            }
                            else
                            {
                                Aji.data[f][g++] = a.data[m][n];
                            }
                        }
                        f++;
                    }
                }
                A_inv.data[j][i] = det_matrix(Aji) * ((i + j) % 2 ? -1 : 1) / det;
                if (A_inv.data[j][i] > -0.005 && A_inv.data[j][i] <= 0.000)
                {
                    A_inv.data[j][i] = 0.00;
                }
            }
        }
        return A_inv;
    }
}

int rank_matrix(Matrix a)
{
    // ToDo
    int rank = 0;
    int rc_min = (a.cols > a.rows ? a.rows : a.cols);
    Matrix A = create_matrix(a.rows, a.cols);
    A = a;
    int i, j, k, m, n, temp;
    for (i = 0; i < rc_min; i++)
    {
        // 判断对角线上元素是否为0，为0则找该元素对应的n-i+1阶子式中是否有非元素并移到对角线上
        if (A.data[i][i] == 0)
        {
            int flag = 1;
            for (m = i; m < A.rows; m++)
            {
                for (n = i; n < A.cols; n++)
                {
                    if (A.data[m][n] != 0)
                    {
                        // 列变换
                        if (n != i)
                        {
                            for (j = i; j < A.rows; j++)
                            {
                                temp = A.data[j][i];
                                A.data[j][i] = A.data[j][n];
                                A.data[j][n] = temp;
                            }
                        }
                        // 行变换
                        if (m != i)
                        {
                            for (j = i; j < A.cols; j++)
                            {
                                temp = A.data[i][j];
                                A.data[i][j] = A.data[m][j];
                                A.data[m][j] = temp;
                            }
                        }
                        // 跳出一重循环
                        flag = 0;
                        break;
                    }
                }
                // 跳出二重循环
                if (flag == 0)
                {
                    break;
                }
            }
            // 若子式全为0则return
            if (flag)
            {
                return rank;
            }
        }
        // 高斯消去
        for (j = i + 1; j < A.rows; j++)
        {
            int f = 1;
            for (k = i; k < A.cols; k++)
            {
                // 判断第i列元素是否为0，为0不需要进行倍加
                if (A.data[j][i] == 0 && f)
                {
                    f = 0;
                    break;
                }
                A.data[j][k] -= A.data[i][k] * A.data[j][i] / A.data[i][i];
            }
        }
        rank++;
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