#include <ft2build.h>
#include FT_FREETYPE_H

// Helper function to get bitmap info from C FreeType
void get_bitmap_info(FT_Face face, int* width, int* height, int* pitch, unsigned char** buffer_ptr) {
    FT_GlyphSlot slot = face->glyph;
    FT_Bitmap* bitmap = &slot->bitmap;
    
    *width = bitmap->width;
    *height = bitmap->rows;
    *pitch = bitmap->pitch;
    *buffer_ptr = bitmap->buffer;
}