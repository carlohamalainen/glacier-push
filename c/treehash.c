#include <assert.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <openssl/evp.h>
#include <openssl/hmac.h>

#define ONE_MB 1048576

#define DIGEST_SIZE 32  // SHA256 digest

// https://wiki.openssl.org/index.php/EVP_Signing_and_Verifying#Calculating_HMAC
void HMAC_sha256(unsigned char *msg, size_t mlen, unsigned char **val, size_t *vlen, EVP_PKEY *pkey)
{
    const EVP_MD *md;
    EVP_MD_CTX *ctx;

    ctx = EVP_MD_CTX_new();

    md = EVP_get_digestbyname("SHA256");
    assert(md != NULL);

    EVP_DigestInit_ex(ctx, md, NULL);

    EVP_DigestSignInit(ctx, NULL, md, NULL, pkey);
    EVP_DigestSignUpdate(ctx, msg, mlen);

    size_t req = 0;
    EVP_DigestSignFinal(ctx, NULL, &req);
    assert(req > 0);

    *val = OPENSSL_malloc(req);
    assert(*val != NULL);

    *vlen = req;
    EVP_DigestSignFinal(ctx, *val, vlen);
    assert(req == *vlen);
}

void foo()
{
    unsigned char password[] = "password";

    EVP_PKEY *pkey = EVP_PKEY_new_mac_key(EVP_PKEY_HMAC, NULL, password, strlen((char *)password));

    unsigned char msg[] = "hello world";
    size_t mlen = strlen((char *)msg);

    unsigned char *val;
    size_t vlen;

    HMAC_sha256(msg, mlen, &val, &vlen, pkey);

    for(int i = 0; i < vlen; i++)
        printf("%02x", val[i]);
    printf("\n");
}


void sha256(unsigned char *buffer, unsigned int buffer_size, unsigned char *output) 
{
    EVP_MD_CTX *mdctx;
    const EVP_MD *md;
    unsigned int md_len;

    md = EVP_get_digestbyname("SHA256");
    assert(md != NULL);

    mdctx = EVP_MD_CTX_new();
    EVP_DigestInit_ex(mdctx, md, NULL);
    EVP_DigestUpdate(mdctx, buffer, buffer_size);
    EVP_DigestFinal_ex(mdctx, output, &md_len);
    EVP_MD_CTX_free(mdctx);

    assert(md_len == DIGEST_SIZE);
}

// start: first byte
// end:   last byte (inclusive)
char * treehash(char *fname, unsigned long long start, unsigned long long end)
{
    FILE *f = NULL;

    unsigned char buffer[ONE_MB];

    // Must be on a sensible boundary.
    assert(start % ONE_MB == 0);

    f = fopen(fname, "rb");
    assert(f != NULL);

    fseek(f, start, SEEK_SET);
    long long nr_bytes = end - start + 1;

    unsigned int nr_blocks = ((unsigned int) (ceil(((double) nr_bytes)/ONE_MB)));

    unsigned char *digests = malloc(nr_blocks*DIGEST_SIZE);

    size_t nr_read;

    int offset = 0;

    // printf("start:     %lld\n", start);
    // printf("end:       %lld\n", end);
    // printf("nr bytes:  %lld\n", nr_bytes);
    // printf("nr blocks: %u\n",   nr_blocks);

    unsigned int bytes_to_process_here = 0;

    while(1) {
        if (nr_bytes == 0) break;

        if (nr_bytes >= ONE_MB)
            bytes_to_process_here = ONE_MB;
        else
            bytes_to_process_here = nr_bytes;

        nr_read = fread(buffer, 1, bytes_to_process_here, f);

        if (nr_read == 0) break;

        sha256(buffer, bytes_to_process_here, &digests[offset*DIGEST_SIZE]);

        offset++;
        nr_bytes -= bytes_to_process_here;
    }

    fclose(f);
    assert(nr_bytes == 0);

    int nr_blocks_left = nr_blocks;

    // next :: [BS.ByteString] -> [BS.ByteString]
    // next []       = []
    // next [a]      = [a]
    // next (a:b:xs) = sha256 (BS.append a b) : next xs

    unsigned char *next_tmp = malloc(nr_blocks*DIGEST_SIZE);

    while (nr_blocks_left > 1) {
        int pairs_to_process = nr_blocks_left/2;

        // printf("pairs to process: %d ", pairs_to_process);
        // if (nr_blocks_left % 2 == 1)
        //     printf("plus one left over\n");
        // else
        //     printf("\n");

        // sha256 the pairs
        for(unsigned int k = 0; k < pairs_to_process; k++)
            sha256(&digests[2*k*DIGEST_SIZE], 2*DIGEST_SIZE, &next_tmp[k*DIGEST_SIZE]);

        // Copy back into digests.
        for(int i = 0; i < pairs_to_process*DIGEST_SIZE; i++)
            digests[i] = next_tmp[i];

        // If there is a block left over, copy it.
        if (nr_blocks_left % 2 == 1) {
            for(int i = 0; i < DIGEST_SIZE; i++)
                digests[pairs_to_process*DIGEST_SIZE + i]
                    = digests[(nr_blocks_left-1)*DIGEST_SIZE + i];
        }

        if (nr_blocks_left % 2 == 0)
            nr_blocks_left = pairs_to_process;
        else
            nr_blocks_left = pairs_to_process + 1;

    }

    // printf("\n");
    // printf("\n");
    char *result = malloc(2*DIGEST_SIZE);
    assert(result != 0);

    for (int i = 0; i < DIGEST_SIZE; i++)
        sprintf(&result[2*i], "%02x", digests[i]);

    free(digests);
    free(next_tmp);

    return(result);
}

