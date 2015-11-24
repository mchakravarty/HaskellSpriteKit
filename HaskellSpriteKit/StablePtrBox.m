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

/*
- (instancetype)initWithCoder:(NSCoder *)decoder
{
  self = [super init];
  if (self) {
    _stablePtr = (void *)[decoder decodeInt64ForKey:@"SPBStablePtr"];
  }
  return self;
}
*/

- (void)dealloc
{
  hs_free_stable_ptr(_stablePtr);
}

/*
- (id)copyWithZone:(NSZone *)zone
{
  return [[StablePtrBox allocWithZone:zone] initWithStablePtr:self.stablePtr];
}

- (void)encodeWithCoder:(NSCoder *)encoder
{
  [encoder encodeInt64:(long)self.stablePtr forKey:@"SPBStablePtr"];
}
*/

@end
