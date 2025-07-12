#include <stdio.h>
#include <stdlib.h>
#include <ft2build.h>
#include FT_FREETYPE_H
#include FT_GLYPH_H

int main() {
    FT_Library library;
    FT_Face face;
    FT_Error error;
    FT_UInt glyph_index;
    
    printf("DETAILED C FREETYPE VALIDATION\n");
    printf("==============================\n\n");
    
    // Step 1: Initialize
    error = FT_Init_FreeType(&library);
    if (error) {
        printf("STEP 1 FAILED: FT_Init_FreeType: %d\n", error);
        return 1;
    }
    printf("STEP 1 SUCCESS: FT_Init_FreeType\n");
    
    // Step 2: Load face
    error = FT_New_Face(library, "/usr/share/fonts/TTF/DejaVuSans.ttf", 0, &face);
    if (error) {
        printf("STEP 2 FAILED: FT_New_Face: %d\n", error);
        return 1;
    }
    printf("STEP 2 SUCCESS: FT_New_Face\n");
    printf("  face->units_per_EM: %ld\n", face->units_per_EM);
    printf("  face->num_glyphs: %ld\n", face->num_glyphs);
    printf("  face->bbox: (%ld,%ld) to (%ld,%ld)\n", 
           face->bbox.xMin, face->bbox.yMin, face->bbox.xMax, face->bbox.yMax);
    
    // Step 3: Get glyph index
    glyph_index = FT_Get_Char_Index(face, 'A');
    printf("\nSTEP 3 SUCCESS: FT_Get_Char_Index('A'): %u\n", glyph_index);
    
    // Step 4: Load glyph (WITHOUT scaling)
    error = FT_Load_Glyph(face, glyph_index, FT_LOAD_NO_SCALE | FT_LOAD_NO_HINTING);
    if (error) {
        printf("STEP 4 FAILED: FT_Load_Glyph: %d\n", error);
        return 1;
    }
    printf("STEP 4 SUCCESS: FT_Load_Glyph (no scale, no hinting)\n");
    
    // Step 5: Examine raw outline data
    FT_Outline* outline = &face->glyph->outline;
    printf("\nSTEP 5: Raw outline data:\n");
    printf("  n_points: %d\n", outline->n_points);
    printf("  n_contours: %d\n", outline->n_contours);
    printf("  flags: 0x%x\n", outline->flags);
    
    printf("  All points (font units):\n");
    for (int i = 0; i < outline->n_points; i++) {
        printf("    Point %2d: (%6ld, %6ld) tag: 0x%02x (%s)\n", 
               i, outline->points[i].x, outline->points[i].y, 
               outline->tags[i],
               (outline->tags[i] & 1) ? "ON" : "OFF");
    }
    
    printf("  Contour endpoints:\n");
    for (int i = 0; i < outline->n_contours; i++) {
        printf("    Contour %d: ends at point %d (points %d-%d)\n", 
               i, outline->contours[i],
               (i == 0) ? 0 : outline->contours[i-1] + 1,
               outline->contours[i]);
    }
    
    // Step 6: Set pixel size and reload WITH scaling
    error = FT_Set_Pixel_Sizes(face, 64, 64);
    if (error) {
        printf("STEP 6 FAILED: FT_Set_Pixel_Sizes: %d\n", error);
        return 1;
    }
    printf("\nSTEP 6 SUCCESS: FT_Set_Pixel_Sizes(64, 64)\n");
    
    error = FT_Load_Glyph(face, glyph_index, FT_LOAD_DEFAULT);
    if (error) {
        printf("STEP 6 FAILED: FT_Load_Glyph (scaled): %d\n", error);
        return 1;
    }
    printf("STEP 6 SUCCESS: FT_Load_Glyph (scaled)\n");
    
    // Step 7: Examine scaled outline
    outline = &face->glyph->outline;
    printf("\nSTEP 7: Scaled outline data:\n");
    printf("  n_points: %d\n", outline->n_points);
    printf("  n_contours: %d\n", outline->n_contours);
    
    printf("  All scaled points (26.6 fixed point):\n");
    for (int i = 0; i < outline->n_points; i++) {
        printf("    Point %2d: (%6ld, %6ld) = (%.2f, %.2f) pixels, tag: 0x%02x\n", 
               i, outline->points[i].x, outline->points[i].y,
               outline->points[i].x / 64.0, outline->points[i].y / 64.0,
               outline->tags[i]);
    }
    
    // Step 8: Render to bitmap
    error = FT_Render_Glyph(face->glyph, FT_RENDER_MODE_NORMAL);
    if (error) {
        printf("STEP 8 FAILED: FT_Render_Glyph: %d\n", error);
        return 1;
    }
    printf("\nSTEP 8 SUCCESS: FT_Render_Glyph\n");
    
    FT_Bitmap* bitmap = &face->glyph->bitmap;
    printf("  bitmap.width: %d\n", bitmap->width);
    printf("  bitmap.rows: %d\n", bitmap->rows);
    printf("  bitmap.pitch: %d\n", bitmap->pitch);
    printf("  bitmap.pixel_mode: %d\n", bitmap->pixel_mode);
    printf("  glyph->bitmap_left: %d\n", face->glyph->bitmap_left);
    printf("  glyph->bitmap_top: %d\n", face->glyph->bitmap_top);
    
    // Count pixels
    int pixel_count = 0;
    for (int y = 0; y < bitmap->rows; y++) {
        for (int x = 0; x < bitmap->width; x++) {
            if (bitmap->buffer[y * bitmap->pitch + x] > 0) {
                pixel_count++;
            }
        }
    }
    printf("  Non-zero pixels: %d / %d\n", pixel_count, bitmap->width * bitmap->rows);
    
    // Save detailed info to file
    FILE* file = fopen("c_validation_details.txt", "w");
    if (file) {
        fprintf(file, "C FreeType Validation Details\n");
        fprintf(file, "=============================\n\n");
        
        fprintf(file, "Font: /usr/share/fonts/TTF/DejaVuSans.ttf\n");
        fprintf(file, "Character: 'A' (glyph index %u)\n", glyph_index);
        fprintf(file, "Units per EM: %ld\n", face->units_per_EM);
        fprintf(file, "Pixel size: 64x64\n\n");
        
        // Re-load unscaled for comparison
        FT_Load_Glyph(face, glyph_index, FT_LOAD_NO_SCALE | FT_LOAD_NO_HINTING);
        FT_Outline* raw_outline = &face->glyph->outline;
        
        fprintf(file, "RAW OUTLINE DATA (font units):\n");
        fprintf(file, "Points: %d\n", raw_outline->n_points);
        fprintf(file, "Contours: %d\n", raw_outline->n_contours);
        
        for (int i = 0; i < raw_outline->n_points; i++) {
            fprintf(file, "Point %2d: (%6ld, %6ld) tag: 0x%02x\n", 
                   i, raw_outline->points[i].x, raw_outline->points[i].y, raw_outline->tags[i]);
        }
        
        for (int i = 0; i < raw_outline->n_contours; i++) {
            fprintf(file, "Contour %d: ends at point %d\n", i, raw_outline->contours[i]);
        }
        
        // Re-load scaled
        FT_Load_Glyph(face, glyph_index, FT_LOAD_DEFAULT);
        FT_Outline* scaled_outline = &face->glyph->outline;
        
        fprintf(file, "\nSCALED OUTLINE DATA (26.6 fixed point):\n");
        fprintf(file, "Points: %d\n", scaled_outline->n_points);
        fprintf(file, "Contours: %d\n", scaled_outline->n_contours);
        
        for (int i = 0; i < scaled_outline->n_points; i++) {
            fprintf(file, "Point %2d: (%6ld, %6ld) = (%.2f, %.2f) pixels, tag: 0x%02x\n", 
                   i, scaled_outline->points[i].x, scaled_outline->points[i].y,
                   scaled_outline->points[i].x / 64.0, scaled_outline->points[i].y / 64.0,
                   scaled_outline->tags[i]);
        }
        
        fclose(file);
        printf("\nDetailed validation saved to: c_validation_details.txt\n");
    }
    
    // Cleanup
    FT_Done_Face(face);
    FT_Done_FreeType(library);
    
    printf("\nC VALIDATION COMPLETE!\n");
    return 0;
}