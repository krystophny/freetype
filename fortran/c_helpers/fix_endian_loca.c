#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <ft2build.h>
#include FT_FREETYPE_H
#include FT_TRUETYPE_TABLES_H

// Convert big-endian 32-bit value to host order
uint32_t be32_to_host(uint32_t be_val) {
    uint8_t* bytes = (uint8_t*)&be_val;
    return (bytes[0] << 24) | (bytes[1] << 16) | (bytes[2] << 8) | bytes[3];
}

int main() {
    FT_Library library;
    FT_Face face;
    FT_Error error;
    
    printf("CORRECTED GLYPH 36 ANALYSIS\n");
    printf("===========================\n\n");
    
    // Initialize FreeType
    error = FT_Init_FreeType(&library);
    if (error) return 1;
    
    error = FT_New_Face(library, "/usr/share/fonts/TTF/DejaVuSans.ttf", 0, &face);
    if (error) {
        FT_Done_FreeType(library);
        return 1;
    }
    
    // Get loca table
    FT_ULong loca_length = 0;
    error = FT_Load_Sfnt_Table(face, FT_MAKE_TAG('l','o','c','a'), 0, NULL, &loca_length);
    if (error) {
        printf("ERROR: Cannot get loca table size\n");
        FT_Done_Face(face);
        FT_Done_FreeType(library);
        return 1;
    }
    
    unsigned char* loca_data = malloc(loca_length);
    error = FT_Load_Sfnt_Table(face, FT_MAKE_TAG('l','o','c','a'), 0, loca_data, &loca_length);
    if (error) {
        printf("ERROR: Cannot read loca table\n");
        free(loca_data);
        FT_Done_Face(face);
        FT_Done_FreeType(library);
        return 1;
    }
    
    // Show raw bytes around glyph 36
    printf("Raw bytes at glyph 36 position (offset %d):\n", 36 * 4);
    for (int i = 0; i < 16; i++) {
        int offset = 36 * 4 + i;
        if (offset < loca_length) {
            printf("%02X ", loca_data[offset]);
        }
        if (i % 8 == 7) printf("\n");
    }
    
    // Interpret as LONG format with correct big-endian conversion
    uint32_t* loca_longs = (uint32_t*)loca_data;
    uint32_t offset36 = be32_to_host(loca_longs[36]);
    uint32_t offset37 = be32_to_host(loca_longs[37]);
    
    printf("\nLoca table entries (LONG format, big-endian corrected):\n");
    printf("  Raw loca[36] = 0x%08X -> corrected = %u\n", loca_longs[36], offset36);
    printf("  Raw loca[37] = 0x%08X -> corrected = %u\n", loca_longs[37], offset37);
    printf("  Glyph 36 size = %u bytes\n", offset37 - offset36);
    
    // Compare with C FreeType results
    printf("\nFT_Load_Glyph(36) results:\n");
    error = FT_Load_Glyph(face, 36, FT_LOAD_NO_SCALE | FT_LOAD_NO_HINTING);
    if (!error) {
        FT_Outline* outline = &face->glyph->outline;
        printf("  Points: %d\n", outline->n_points);
        printf("  Contours: %d\n", outline->n_contours);
        printf("  First point: (%ld, %ld)\n", 
               outline->n_points > 0 ? outline->points[0].x : 0,
               outline->n_points > 0 ? outline->points[0].y : 0);
    }
    
    free(loca_data);
    FT_Done_Face(face);
    FT_Done_FreeType(library);
    
    return 0;
}