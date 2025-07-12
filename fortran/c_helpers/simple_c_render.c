#include <stdio.h>
#include <stdlib.h>
#include <ft2build.h>
#include FT_FREETYPE_H

int main() {
    FT_Library library;
    FT_Face face;
    FT_Error error;
    FT_UInt glyph_index;
    
    printf("C FreeType Reference Implementation\n");
    printf("==================================\n\n");
    
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
    
    printf("Font loaded successfully!\n");
    printf("  Units per EM: %ld\n", face->units_per_EM);
    printf("  Number of glyphs: %ld\n", face->num_glyphs);
    
    // Set pixel size
    error = FT_Set_Pixel_Sizes(face, 64, 64);
    if (error) {
        printf("ERROR: FT_Set_Pixel_Sizes failed: %d\n", error);
        FT_Done_Face(face);
        FT_Done_FreeType(library);
        return 1;
    }
    
    // Get glyph index for 'A'
    glyph_index = FT_Get_Char_Index(face, 'A');
    printf("\nGlyph index for 'A': %u\n", glyph_index);
    
    // Load glyph
    error = FT_Load_Glyph(face, glyph_index, FT_LOAD_DEFAULT);
    if (error) {
        printf("ERROR: FT_Load_Glyph failed: %d\n", error);
        FT_Done_Face(face);
        FT_Done_FreeType(library);
        return 1;
    }
    
    printf("Glyph loaded successfully!\n");
    
    // Print outline info
    FT_Outline* outline = &face->glyph->outline;
    printf("  Points: %d\n", outline->n_points);
    printf("  Contours: %d\n", outline->n_contours);
    
    printf("  First 5 points:\n");
    for (int i = 0; i < 5 && i < outline->n_points; i++) {
        printf("    Point %d: (%ld, %ld) tag: %d\n", 
               i+1, outline->points[i].x, outline->points[i].y, outline->tags[i]);
    }
    
    printf("  Contour endpoints:\n");
    for (int i = 0; i < outline->n_contours; i++) {
        printf("    Contour %d ends at point: %d\n", i+1, outline->contours[i]);
    }
    
    // Render glyph
    error = FT_Render_Glyph(face->glyph, FT_RENDER_MODE_NORMAL);
    if (error) {
        printf("ERROR: FT_Render_Glyph failed: %d\n", error);
        FT_Done_Face(face);
        FT_Done_FreeType(library);
        return 1;
    }
    
    printf("\nGlyph rendered successfully!\n");
    FT_Bitmap* bitmap = &face->glyph->bitmap;
    printf("  Bitmap size: %dx%d\n", bitmap->width, bitmap->rows);
    printf("  Bitmap pitch: %d\n", bitmap->pitch);
    
    // Count non-zero pixels
    int pixel_count = 0;
    int max_val = 0;
    for (int y = 0; y < bitmap->rows; y++) {
        for (int x = 0; x < bitmap->width; x++) {
            unsigned char pixel = bitmap->buffer[y * bitmap->pitch + x];
            if (pixel > 0) {
                pixel_count++;
                if (pixel > max_val) max_val = pixel;
            }
        }
    }
    
    printf("  Non-zero pixels: %d / %d\n", pixel_count, bitmap->width * bitmap->rows);
    printf("  Max pixel value: %d\n", max_val);
    
    // ASCII preview
    printf("\nASCII preview (16x16):\n");
    for (int y = 0; y < 16 && y < bitmap->rows; y++) {
        printf("  ");
        for (int x = 0; x < 16 && x < bitmap->width; x++) {
            unsigned char pixel = bitmap->buffer[y * bitmap->pitch + x];
            char c;
            if (pixel > 192) c = '#';
            else if (pixel > 128) c = '*';
            else if (pixel > 64) c = '.';
            else if (pixel > 0) c = ',';
            else c = ' ';
            printf("%c", c);
        }
        printf("\n");
    }
    
    // Save bitmap to PGM
    FILE* file = fopen("c_reference_render.pgm", "w");
    if (file) {
        fprintf(file, "P2\n");
        fprintf(file, "# C FreeType reference render\n");
        fprintf(file, "%d %d\n", bitmap->width, bitmap->rows);
        fprintf(file, "255\n");
        
        for (int y = 0; y < bitmap->rows; y++) {
            for (int x = 0; x < bitmap->width; x++) {
                fprintf(file, "%d ", bitmap->buffer[y * bitmap->pitch + x]);
            }
            fprintf(file, "\n");
        }
        fclose(file);
        printf("\nSaved: c_reference_render.pgm\n");
    }
    
    // Cleanup
    FT_Done_Face(face);
    FT_Done_FreeType(library);
    
    printf("\nC reference rendering complete!\n");
    return 0;
}