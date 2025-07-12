#include <stdio.h>
#include <stdlib.h>
#include <ft2build.h>
#include FT_FREETYPE_H
#include FT_TRUETYPE_TABLES_H

void print_hex_bytes(unsigned char* data, int count, int start_offset) {
    printf("Raw bytes at offset %d:\n", start_offset);
    for (int i = 0; i < count; i++) {
        if (i % 16 == 0) printf("  %04X: ", start_offset + i);
        printf("%02X ", data[i]);
        if (i % 16 == 15) printf("\n");
    }
    if (count % 16 != 0) printf("\n");
}

int main() {
    FT_Library library;
    FT_Face face;
    FT_Error error;
    
    printf("LOCA TABLE BYTE ANALYSIS\n");
    printf("========================\n\n");
    
    // Initialize FreeType
    error = FT_Init_FreeType(&library);
    if (error) return 1;
    
    error = FT_New_Face(library, "/usr/share/fonts/TTF/DejaVuSans.ttf", 0, &face);
    if (error) {
        FT_Done_FreeType(library);
        return 1;
    }
    
    // Get head table
    TT_Header* head = (TT_Header*)FT_Get_Sfnt_Table(face, FT_SFNT_HEAD);
    if (head) {
        printf("Index_To_Loc_Format: %d (%s)\n", 
               head->Index_To_Loc_Format,
               head->Index_To_Loc_Format == 0 ? "SHORT" : "LONG");
    }
    
    // Get loca table size
    FT_ULong loca_length = 0;
    error = FT_Load_Sfnt_Table(face, FT_MAKE_TAG('l','o','c','a'), 0, NULL, &loca_length);
    if (error) {
        printf("ERROR: Cannot get loca table size\n");
        FT_Done_Face(face);
        FT_Done_FreeType(library);
        return 1;
    }
    
    printf("Loca table size: %lu bytes\n", loca_length);
    
    // Read loca table
    unsigned char* loca_data = malloc(loca_length);
    if (!loca_data) {
        printf("ERROR: Memory allocation failed\n");
        FT_Done_Face(face);
        FT_Done_FreeType(library);
        return 1;
    }
    
    error = FT_Load_Sfnt_Table(face, FT_MAKE_TAG('l','o','c','a'), 0, loca_data, &loca_length);
    if (error) {
        printf("ERROR: Cannot read loca table\n");
        free(loca_data);
        FT_Done_Face(face);
        FT_Done_FreeType(library);
        return 1;
    }
    
    // Show first 64 bytes
    print_hex_bytes(loca_data, 64, 0);
    
    // Show bytes around glyph 36 (index 36 in LONG format = byte offset 36*4 = 144)
    printf("\nBytes around glyph 36 (LONG format interpretation):\n");
    int glyph36_offset = 36 * 4;
    if (glyph36_offset + 16 < loca_length) {
        print_hex_bytes(loca_data + glyph36_offset - 8, 24, glyph36_offset - 8);
    }
    
    // Interpret as LONG format for glyph 36
    if (glyph36_offset + 8 < loca_length) {
        unsigned int* longs = (unsigned int*)loca_data;
        printf("\nGlyph 36 LONG format:\n");
        printf("  loca[36] = 0x%08X = %u\n", longs[36], longs[36]);
        printf("  loca[37] = 0x%08X = %u\n", longs[37], longs[37]);
        printf("  Size = %u bytes\n", longs[37] - longs[36]);
    }
    
    // Now show what happens with glyph 36 loading through FT_Load_Glyph
    printf("\nC FreeType FT_Load_Glyph(36) results:\n");
    error = FT_Load_Glyph(face, 36, FT_LOAD_NO_SCALE | FT_LOAD_NO_HINTING);
    if (!error) {
        FT_Outline* outline = &face->glyph->outline;
        printf("  Points: %d\n", outline->n_points);
        printf("  Contours: %d\n", outline->n_contours);
        if (outline->n_points > 0) {
            printf("  First point: (%ld, %ld)\n", outline->points[0].x, outline->points[0].y);
        }
    }
    
    free(loca_data);
    FT_Done_Face(face);
    FT_Done_FreeType(library);
    
    return 0;
}