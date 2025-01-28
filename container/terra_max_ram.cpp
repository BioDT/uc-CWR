// Workaround for memory issues in terra
#include <stdlib.h>
#include <sys/sysinfo.h>

double availableRAM()
{
    struct sysinfo info;
    sysinfo(&info);
    double ram = info.freeram * info.mem_unit;

    const char* s = getenv("R_TERRA_MAX_RAM_MB");
    double max_ram = (s == NULL) ? 0 : strtol(s, NULL, 10);
    if (max_ram > 0) {
        max_ram *= 1024 * 1024;
        ram = max_ram < ram ? max_ram : ram;
    }
    return ram / 8;
}

