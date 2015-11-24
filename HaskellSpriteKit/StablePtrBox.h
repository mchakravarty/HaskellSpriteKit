//
//  StablePtrBox.h
//  HaskellSpriteKit
//
//  Created by Manuel M T Chakravarty on 5/12/2014.
//  Copyright (c) 2014 Manuel M T Chakravarty. All rights reserved.
//
//  This class wraps stable pointers of the Haskell FFI such that we can use ARC to free the wrapped stable pointer in
//  the Haskell runtime.

#import <Foundation/Foundation.h>
#import "GHC/HsFFI.h"


@interface StablePtrBox : NSObject /* <NSCopying, NSCoding> */

@property (readonly, assign) HsStablePtr stablePtr;

+ (instancetype)stablePtrBox:(HsStablePtr)stablePtr;

- (instancetype)initWithStablePtr:(HsStablePtr)stablePtr;

@end
