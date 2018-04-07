
int sum(int x) {
    if (x < 1) {
        return 1;
    }

    int i;
    int j;
    i = 0;
    j = 0;
    while (i < x) {
        j = sum(i);
    }
    return j;
}

int main() {
    int result;

    result = sum(20);
    print(result);

    return 0;
}
