//
//  StablePtrBox.m
//  HaskellSpriteKit
//
//  Created by Manuel M T Chakravarty on 5/12/2014.
//  Copyright (c) 2014 Manuel M T Chakravarty. All rights reserved.
//

#import "StablePtrBox.h"


@implementation StablePtrBox

+ (instancetype)stablePtrBox:(HsStablePtr)stablePtr
{
  return [[StablePtrBox alloc] initWithStablePtr:stablePtr];
}

- (instancetype)initWithStablePtr:(HsStablePtr)stablePtr
{
  self = [super init];
  if (self)
    _stablePtr = stablePtr;
  return self;
}

- (void)dealloc
{
  hs_free_stable_ptr(_stablePtr);
}

@end
