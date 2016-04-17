#ifndef GOTO_HPP
#define GOTO_HPP

#include <vector>

template<typename T>
int len(const T &t)
{
    return t.size();
}

template<typename T, typename S>
std::vector<T> take_slice(const S &arr, int start, int end, int cap)
{
    std::vector<T> t;
    if(start == -1)
        start = 0;
    if(end == -1)
        end = arr.size();
    if(cap == -1)
        cap = arr.size();
    for(int i = start; i < end; i++)
    {
        t.push_back(arr[i]);
    }
    return t;
}

template<typename T>
std::vector<T>& append(std::vector<T> & vec, T x)
{
    vec.push_back(x);
    return vec;
}

#endif
