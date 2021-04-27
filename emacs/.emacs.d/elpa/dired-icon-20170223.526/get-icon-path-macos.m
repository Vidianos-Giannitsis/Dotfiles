// Copyright (C) 2017  Shuai Zhao <slege_tank@163.com>
//
// This file is not part of GNU Emacs.
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
//
// Use CoreService framework to get icon for file, and cache it.

#import <Foundation/Foundation.h>
#import <CoreServices/CoreServices.h>
#import <AppKit/AppKit.h>
#import <CommonCrypto/CommonCrypto.h>

NSArray * cachedImageNameArray;
NSString * cachedImageDir;


/**
 Generate NSBitImageRep.

 @param image Input image
 @return ImageRep
 */
NSBitmapImageRep * scaleBitmapImageRep(NSImage * image)
{
    NSBitmapImageRep *rep = [[NSBitmapImageRep alloc]
                             initWithBitmapDataPlanes:NULL
                             pixelsWide:image.size.width
                             pixelsHigh:image.size.height
                             bitsPerSample:8
                             samplesPerPixel:4
                             hasAlpha:YES
                             isPlanar:NO
                             colorSpaceName:NSDeviceRGBColorSpace
                             bytesPerRow:image.size.width*4
                             bitsPerPixel:32];

    NSGraphicsContext * ctx = [NSGraphicsContext graphicsContextWithBitmapImageRep: rep];
    [NSGraphicsContext saveGraphicsState];
    [NSGraphicsContext setCurrentContext:ctx];
    [image drawAtPoint:NSZeroPoint fromRect:NSZeroRect operation:NSCompositingOperationCopy fraction:1.0];
    [ctx flushGraphics];
    [NSGraphicsContext restoreGraphicsState];

    return rep;
}


/**
 Generate MD5 of the imageData.

 @param imageData Image data
 @return MD5
 */
NSString * MD5ForData(NSData * imageData)
{
    unsigned char result[CC_MD5_DIGEST_LENGTH];
    CC_MD5([imageData bytes], (CC_LONG)[imageData length], result);
    return [NSString stringWithFormat:
            @"%02X%02X%02X%02X%02X%02X%02X%02X%02X%02X%02X%02X%02X%02X%02X%02X",
            result[0], result[1], result[2], result[3],
            result[4], result[5], result[6], result[7],
            result[8], result[9], result[10], result[11],
            result[12], result[13], result[14], result[15]
            ];
}


/**
 Get file's icon and cache it.

 @param filePath File's path.
 @param size Image size.
 @return Path of the cache icon.
 */
NSString * IconForFile(NSString * filePath, NSString * size)
{
    NSImage * image = [[NSWorkspace sharedWorkspace] iconForFile:filePath];
    image.size = NSMakeSize([size floatValue], [size floatValue]);

    NSBitmapImageRep * rep = scaleBitmapImageRep(image);

    // Cache image to file
    NSDictionary *imageProps = [NSDictionary dictionaryWithObject:[NSNumber numberWithFloat:1.0] forKey:NSImageCompressionFactor];
    NSData *targetData = [rep representationUsingType:NSBitmapImageFileTypePNG properties:imageProps];
    NSString * MD5 = MD5ForData(targetData);
    NSString * cachePath = [cachedImageDir stringByAppendingPathComponent:MD5];
    if (![cachedImageNameArray containsObject:MD5])
    {
        [targetData writeToFile:cachePath atomically:NO];
    }

    return cachePath;
}

int main(int argc, char *argv[])
{
    if (argc < 4) return 0;
    if (strlen(argv[1]) == 0 || strlen(argv[2]) == 0 || strlen(argv[3]) == 0) return 0;

    NSArray * fileNames = [[[NSString stringWithUTF8String:argv[1]] stringByTrimmingCharactersInSet:[NSCharacterSet whitespaceAndNewlineCharacterSet]] componentsSeparatedByString:@"\n"];

    cachedImageDir = [NSString stringWithUTF8String:argv[2]];

    BOOL isDir = NO;
    if (![[NSFileManager defaultManager] fileExistsAtPath:cachedImageDir isDirectory:&isDir] || !isDir)
    {
        if (![[NSFileManager defaultManager] createDirectoryAtPath:cachedImageDir withIntermediateDirectories:YES attributes:nil error:nil])
        {
            return 0;
        }
    }

    cachedImageNameArray = [[NSFileManager defaultManager] contentsOfDirectoryAtPath:cachedImageDir error:nil];

    NSString * size = [NSString stringWithUTF8String:argv[3]];
    for (NSString * fileName in fileNames)
    {
        printf("%s\n", [IconForFile(fileName, size) UTF8String]);
    }
}
