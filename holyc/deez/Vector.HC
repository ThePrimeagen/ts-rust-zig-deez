// Adapted from https://gitgud.io/CrunkLord420/blazeitfgt

public class Vector
{
  I64 len;
  I64 cap;
  I64 size;
  U8 *d
};

public U0 VectorInit(Vector* vec, I64 size)
{
  vec->len = 0;
  vec->cap = 0;
  vec->size = size;
  vec->d = NULL;
}

public Vector *VectorNew(I64 size, U64 prealloc=0)
{
	Vector *vec = MAlloc(sizeof(Vector));
	VectorInit(vec, size);
	if (prealloc) {
		vec->cap = prealloc;
		vec->d = MAlloc(vec->size * vec->cap);
	}
	return vec;
}

public U0 VectorAdd(Vector* vec, U8* d)
{
  if (vec->cap <= vec->len) {
    if (vec->cap <= 0)
      vec->cap = 1;
    vec->cap = vec->cap * 2;
    U8 *tmpPtr = MAlloc(vec->size*vec->cap);
    if (vec->len > 0)
      MemCpy(tmpPtr, vec->d, vec->size*vec->len);
    Free(vec->d);
    vec->d = tmpPtr;
  }
  MemCpy(&vec->d[vec->len*vec->size], d, vec->size);
  vec->len++;
}

public U0 VectorInc(Vector* vec, I64 amount)
{
  I64 newCap = vec->len + amount;
  if (newCap > vec->cap) {
    vec->cap = newCap;
    U8 *tmpPtr = MAlloc(newCap*vec->size);
    if (vec->len > 0)
      MemCpy(tmpPtr, vec->d, vec->size*vec->len);
    Free(vec->d);
    vec->d = tmpPtr;
  }
  vec->len = newCap;
}

public U0 VectorDel(Vector* vec, I64 index)
{
  if (vec->len > 1)
    MemCpy(&vec->d[index*vec->size], &vec->d[(vec->len-1)*vec->size], vec->size);
  vec->len--;
}

public U0 VectorSetLen(Vector* vec, I64 len)
{
  if (vec->cap < len) {
    vec->cap = len;
    U8 *tmpPtr = MAlloc(len*vec->size);
    if (vec->len > 0)
      MemCpy(tmpPtr, vec->d, vec->size*vec->len);
    Free(vec->d);
    vec->d = tmpPtr;
  }
  vec->len = len;
}

