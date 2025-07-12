#include <stdio.h>
#include <stdlib.h>
#include <ft2build.h>
#include FT_FREETYPE_H
#include FT_TRUETYPE_TABLES_H

int main() {
    FT_Library library;
    FT_Face face;
    FT_Error error;
    
    printf("HEAD TABLE FORMAT CHECK\n");
    printf("=======================\n\n");
    
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
    
    // Get head table
    TT_Header* head = (TT_Header*)FT_Get_Sfnt_Table(face, FT_SFNT_HEAD);
    if (head) {
        printf("HEAD table found:\n");
        printf("  Index_To_Loc_Format: %d\n", head->Index_To_Loc_Format);
        printf("  Units_Per_EM: %d\n", head->Units_Per_EM);
        printf("  Magic_Number: 0x%08X\n", head->Magic_Number);
        
        if (head->Index_To_Loc_Format == 0) {
            printf("  -> SHORT format (16-bit offsets * 2)\n");
        } else if (head->Index_To_Loc_Format == 1) {
            printf("  -> LONG format (32-bit offsets)\n");
        } else {
            printf("  -> UNKNOWN format!\n");
        }
    } else {
        printf("ERROR: Could not get HEAD table\n");
    }
    
    // Clean up
    FT_Done_Face(face);
    FT_Done_FreeType(library);
    
    return 0;
}