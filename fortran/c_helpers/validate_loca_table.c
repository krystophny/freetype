#include <stdio.h>
#include <stdlib.h>
#include <ft2build.h>
#include FT_FREETYPE_H
#include FT_TRUETYPE_TABLES_H

// Function to dump loca table directly
void dump_loca_table_raw(FT_Face face, int glyph_index) {
    printf("C LOCA TABLE VALIDATION\n");
    printf("======================\n\n");
    
    printf("Font info:\n");
    printf("  num_glyphs: %ld\n", face->num_glyphs);
    printf("  units_per_EM: %ld\n", face->units_per_EM);
    
    // Get head table for indexToLocFormat
    TT_Header* head = (TT_Header*)FT_Get_Sfnt_Table(face, FT_SFNT_HEAD);
    short loca_format = 1;  // Default to long format
    if (head) {
        loca_format = head->Index_To_Loc_Format;
        printf("  indexToLocFormat: %d\n", loca_format);
    }
    
    printf("\nGlyph %d analysis:\n", glyph_index);
    
    // Try to access loca table data (this is internal FreeType data)
    // We'll use public API to get offsets
    
    FT_Error error;
    FT_ULong length;
    
    // Get raw glyf table
    error = FT_Load_Sfnt_Table(face, FT_MAKE_TAG('g','l','y','f'), 0, NULL, &length);
    if (error) {
        printf("ERROR: Cannot access glyf table: %d\n", error);
        return;
    }
    printf("  glyf table size: %lu bytes\n", length);
    
    // Get raw loca table
    error = FT_Load_Sfnt_Table(face, FT_MAKE_TAG('l','o','c','a'), 0, NULL, &length);
    if (error) {
        printf("ERROR: Cannot access loca table: %d\n", error);
        return;
    }
    printf("  loca table size: %lu bytes\n", length);
    
    // Allocate and read loca table
    unsigned char* loca_data = malloc(length);
    if (!loca_data) {
        printf("ERROR: Cannot allocate memory for loca table\n");
        return;
    }
    
    error = FT_Load_Sfnt_Table(face, FT_MAKE_TAG('l','o','c','a'), 0, loca_data, &length);
    if (error) {
        printf("ERROR: Cannot read loca table: %d\n", error);
        free(loca_data);
        return;
    }
    
    // Parse loca table based on format
    printf("\nLoca table entries around glyph %d:\n", glyph_index);
    
    if (loca_format == 0) {
        // Short format (16-bit offsets, actual offset = value * 2)
        printf("  Format: SHORT (16-bit entries)\n");
        unsigned short* loca_short = (unsigned short*)loca_data;
        int num_entries = length / 2;
        
        for (int i = glyph_index - 2; i <= glyph_index + 3 && i < num_entries; i++) {
            if (i >= 0) {
                unsigned int offset = ((unsigned int)loca_short[i]) * 2;  // Convert to bytes
                printf("    loca[%3d] = %5u * 2 = %8u\n", i, loca_short[i], offset);
                
                if (i == glyph_index) {
                    printf("      --> GLYPH %d OFFSET: %u\n", glyph_index, offset);
                }
                if (i == glyph_index + 1) {
                    unsigned int prev_offset = ((unsigned int)loca_short[glyph_index]) * 2;
                    unsigned int size = offset - prev_offset;
                    printf("      --> GLYPH %d SIZE: %u bytes\n", glyph_index, size);
                }
            }
        }
    } else {
        // Long format (32-bit offsets)
        printf("  Format: LONG (32-bit entries)\n");
        unsigned int* loca_long = (unsigned int*)loca_data;
        int num_entries = length / 4;
        
        for (int i = glyph_index - 2; i <= glyph_index + 3 && i < num_entries; i++) {
            if (i >= 0) {
                unsigned int offset = loca_long[i];
                printf("    loca[%3d] = %8u\n", i, offset);
                
                if (i == glyph_index) {
                    printf("      --> GLYPH %d OFFSET: %u\n", glyph_index, offset);
                }
                if (i == glyph_index + 1) {
                    unsigned int size = offset - loca_long[glyph_index];
                    printf("      --> GLYPH %d SIZE: %u bytes\n", glyph_index, size);
                }
            }
        }
    }
    
    free(loca_data);
}

int main() {
    FT_Library library;
    FT_Face face;
    FT_Error error;
    
    // Initialize FreeType
    error = FT_Init_FreeType(&library);
    if (error) {
        printf("ERROR: FT_Init_FreeType failed: %d\n", error);
        return 1;
    }
    
    // Load font
    error = FT_New_Face(library, "/usr/share/fonts/TTF/DejaVuSans.ttf", 0, &face);
    if (error) {
        printf("ERROR: FT_New_Face failed: %d\n", error);
        FT_Done_FreeType(library);
        return 1;
    }
    
    // Validate loca table for glyph 'A' (index 36)
    dump_loca_table_raw(face, 36);
    
    printf("\nNow loading glyph 36 with FT_Load_Glyph to see what C actually loads:\n");
    
    // Load glyph without scaling or hinting
    error = FT_Load_Glyph(face, 36, FT_LOAD_NO_SCALE | FT_LOAD_NO_HINTING);
    if (error) {
        printf("ERROR: FT_Load_Glyph failed: %d\n", error);
    } else {
        FT_Outline* outline = &face->glyph->outline;
        printf("  C FreeType loaded:\n");
        printf("    Points: %d\n", outline->n_points);
        printf("    Contours: %d\n", outline->n_contours);
        printf("    First point: (%ld, %ld)\n", 
               outline->n_points > 0 ? outline->points[0].x : 0,
               outline->n_points > 0 ? outline->points[0].y : 0);
    }
    
    // Save detailed loca info to file
    FILE* file = fopen("c_loca_validation.txt", "w");
    if (file) {
        fprintf(file, "C FreeType Loca Table Validation\n");
        fprintf(file, "================================\n\n");
        fprintf(file, "Font: /usr/share/fonts/TTF/DejaVuSans.ttf\n");
        fprintf(file, "Target glyph: 36 ('A')\n\n");
        
        // Load and save complete loca table
        FT_ULong length;
        FT_Load_Sfnt_Table(face, FT_MAKE_TAG('l','o','c','a'), 0, NULL, &length);
        
        unsigned char* loca_data = malloc(length);
        if (loca_data) {
            FT_Load_Sfnt_Table(face, FT_MAKE_TAG('l','o','c','a'), 0, loca_data, &length);
            
            TT_Header* head2 = (TT_Header*)FT_Get_Sfnt_Table(face, FT_SFNT_HEAD);
            short loca_format2 = 1;
            if (head2) loca_format2 = head2->Index_To_Loc_Format;
            
            fprintf(file, "Loca table format: %s\n", 
                   loca_format2 == 0 ? "SHORT" : "LONG");
            fprintf(file, "Loca table size: %lu bytes\n", length);
            fprintf(file, "Number of entries: %lu\n", 
                   loca_format2 == 0 ? length/2 : length/4);
            
            fprintf(file, "\nFirst 50 loca entries:\n");
            
            if (loca_format2 == 0) {
                unsigned short* loca_short = (unsigned short*)loca_data;
                for (int i = 0; i < 50 && i < length/2; i++) {
                    unsigned int offset = ((unsigned int)loca_short[i]) * 2;
                    fprintf(file, "loca[%2d] = %5u * 2 = %8u\n", i, loca_short[i], offset);
                }
            } else {
                unsigned int* loca_long = (unsigned int*)loca_data;
                for (int i = 0; i < 50 && i < length/4; i++) {
                    fprintf(file, "loca[%2d] = %8u\n", i, loca_long[i]);
                }
            }
            
            free(loca_data);
        }
        
        fclose(file);
        printf("\nDetailed loca table saved to: c_loca_validation.txt\n");
    }
    
    // Cleanup
    FT_Done_Face(face);
    FT_Done_FreeType(library);
    
    return 0;
}