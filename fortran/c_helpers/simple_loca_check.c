#include <stdio.h>
#include <stdlib.h>
#include <ft2build.h>
#include FT_FREETYPE_H
#include FT_TRUETYPE_TABLES_H

int main() {
    FT_Library library;
    FT_Face face;
    FT_Error error;
    
    printf("SIMPLE C LOCA TABLE CHECK\n");
    printf("=========================\n\n");
    
    // Initialize and load
    error = FT_Init_FreeType(&library);
    if (error) return 1;
    
    error = FT_New_Face(library, "/usr/share/fonts/TTF/DejaVuSans.ttf", 0, &face);
    if (error) {
        FT_Done_FreeType(library);
        return 1;
    }
    
    printf("Font loaded successfully\n");
    printf("num_glyphs: %ld\n", face->num_glyphs);
    
    // Test raw table access
    FT_ULong loca_length = 0;
    error = FT_Load_Sfnt_Table(face, FT_MAKE_TAG('l','o','c','a'), 0, NULL, &loca_length);
    if (error) {
        printf("ERROR: Cannot access loca table: %d\n", error);
    } else {
        printf("loca table size: %lu bytes\n", loca_length);
        
        // Read just the first few entries to see the format
        unsigned char buffer[200];  // Enough for ~50 short entries or 25 long entries
        FT_ULong read_length = sizeof(buffer);
        if (read_length > loca_length) read_length = loca_length;
        
        error = FT_Load_Sfnt_Table(face, FT_MAKE_TAG('l','o','c','a'), 0, buffer, &read_length);
        if (!error) {
            printf("Successfully read %lu bytes from loca table\n", read_length);
            
            // Try to determine format by checking if values make sense as short vs long
            unsigned short* as_short = (unsigned short*)buffer;
            unsigned int* as_long = (unsigned int*)buffer;
            
            printf("\nFirst few entries interpreted as SHORT format:\n");
            for (int i = 0; i < 10 && i*2 < read_length; i++) {
                printf("  loca[%d] = %u (offset %u)\n", i, as_short[i], as_short[i] * 2);
            }
            
            printf("\nFirst few entries interpreted as LONG format:\n");
            for (int i = 0; i < 5 && i*4 < read_length; i++) {
                printf("  loca[%d] = %u\n", i, as_long[i]);
            }
            
            // Check specific glyph 36
            printf("\nGlyph 36 analysis:\n");
            if (36*2 + 4 <= read_length) {
                unsigned int offset_short = as_short[36] * 2;
                unsigned int next_offset_short = as_short[37] * 2;
                printf("  SHORT format: offset=%u, size=%u\n", 
                       offset_short, next_offset_short - offset_short);
            }
            
            if (36*4 + 8 <= read_length) {
                unsigned int offset_long = as_long[36];
                unsigned int next_offset_long = as_long[37];
                printf("  LONG format: offset=%u, size=%u\n", 
                       offset_long, next_offset_long - offset_long);
            }
        }
    }
    
    // Test what FT_Load_Glyph actually returns for comparison
    printf("\nFT_Load_Glyph(36) returns:\n");
    error = FT_Load_Glyph(face, 36, FT_LOAD_NO_SCALE | FT_LOAD_NO_HINTING);
    if (!error) {
        FT_Outline* outline = &face->glyph->outline;
        printf("  points: %d\n", outline->n_points);
        printf("  contours: %d\n", outline->n_contours);
        if (outline->n_points > 0) {
            printf("  first point: (%ld, %ld)\n", outline->points[0].x, outline->points[0].y);
        }
    } else {
        printf("  ERROR: %d\n", error);
    }
    
    // Save analysis to file
    FILE* file = fopen("simple_loca_analysis.txt", "w");
    if (file) {
        fprintf(file, "Loca table analysis for glyph 36:\n");
        fprintf(file, "Font: /usr/share/fonts/TTF/DejaVuSans.ttf\n");
        fprintf(file, "Loca table size: %lu bytes\n", loca_length);
        
        // Re-read for file output
        unsigned char* full_buffer = malloc(loca_length);
        if (full_buffer) {
            FT_Load_Sfnt_Table(face, FT_MAKE_TAG('l','o','c','a'), 0, full_buffer, &loca_length);
            
            fprintf(file, "\nAll loca entries (first 100):\n");
            if (loca_length >= 200) {  // Assume short format if small
                unsigned short* shorts = (unsigned short*)full_buffer;
                fprintf(file, "Interpreting as SHORT format:\n");
                for (int i = 0; i < 100 && i*2 < loca_length; i++) {
                    fprintf(file, "loca[%3d] = %5u (offset %8u)\n", i, shorts[i], shorts[i] * 2);
                }
            }
            
            if (loca_length >= 400) {  // Could be long format
                unsigned int* longs = (unsigned int*)full_buffer;
                fprintf(file, "\nInterpreting as LONG format:\n");
                for (int i = 0; i < 100 && i*4 < loca_length; i++) {
                    fprintf(file, "loca[%3d] = %8u\n", i, longs[i]);
                }
            }
            
            free(full_buffer);
        }
        
        fclose(file);
        printf("\nFull analysis saved to: simple_loca_analysis.txt\n");
    }
    
    FT_Done_Face(face);
    FT_Done_FreeType(library);
    
    printf("\nDone.\n");
    return 0;
}