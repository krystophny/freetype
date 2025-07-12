#include <stdio.h>
#include <stdlib.h>
#include <ft2build.h>
#include FT_FREETYPE_H
#include FT_TRUETYPE_TABLES_H

int main() {
    FT_Library library;
    FT_Face face;
    FT_Error error;
    
    printf("EXACT GLYPH 36 ANALYSIS\n");
    printf("=======================\n\n");
    
    // Initialize FreeType
    error = FT_Init_FreeType(&library);
    if (error) return 1;
    
    error = FT_New_Face(library, "/usr/share/fonts/TTF/DejaVuSans.ttf", 0, &face);
    if (error) {
        FT_Done_FreeType(library);
        return 1;
    }
    
    // Get head table to show format
    TT_Header* head = (TT_Header*)FT_Get_Sfnt_Table(face, FT_SFNT_HEAD);
    if (head) {
        printf("HEAD table indexToLocFormat: %d (%s)\n", 
               head->Index_To_Loc_Format,
               head->Index_To_Loc_Format == 0 ? "SHORT" : "LONG");
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
    
    // Interpret as LONG format (since head says format=1)
    unsigned int* loca_longs = (unsigned int*)loca_data;
    printf("\nLoca table entries (LONG format):\n");
    printf("  loca[36] = 0x%08X = %u\n", loca_longs[36], loca_longs[36]);
    printf("  loca[37] = 0x%08X = %u\n", loca_longs[37], loca_longs[37]);
    printf("  Glyph 36 size = %u bytes\n", loca_longs[37] - loca_longs[36]);
    
    // Now show what FT_Load_Glyph returns
    printf("\nFT_Load_Glyph(36) results:\n");
    error = FT_Load_Glyph(face, 36, FT_LOAD_NO_SCALE | FT_LOAD_NO_HINTING);
    if (!error) {
        FT_Outline* outline = &face->glyph->outline;
        printf("  Points: %d\n", outline->n_points);
        printf("  Contours: %d\n", outline->n_contours);
        
        printf("  First 5 coordinates:\n");
        for (int i = 0; i < outline->n_points && i < 5; i++) {
            printf("    Point %d: (%ld, %ld)\n", i, outline->points[i].x, outline->points[i].y);
        }
        
        if (outline->n_contours > 0) {
            printf("  Contour endpoints:\n");
            for (int i = 0; i < outline->n_contours; i++) {
                printf("    Contour %d: ends at point %d\n", i, outline->contours[i]);
            }
        }
    }
    
    free(loca_data);
    FT_Done_Face(face);
    FT_Done_FreeType(library);
    
    return 0;
}