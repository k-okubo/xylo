
#include "xylo/runtime/memory.h"

#include <gc/gc.h>

#include <iostream>


void* xylo_malloc(size_t size) {
  std::cout << "xylo_malloc: " << size << " bytes\n";
  return GC_MALLOC(size);
}
