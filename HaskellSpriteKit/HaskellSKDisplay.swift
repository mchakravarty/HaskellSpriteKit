//
//  HaskellSKDisplay.swift
//  HaskellSpriteKit
//
//  Created by Manuel M T Chakravarty on 28/10/2014.
//  Copyright (c) 2014 Manuel M T Chakravarty. All rights reserved.
//

import Foundation
import SpriteKit


// Must be called, for now, before using any HaskellSpriteKit functions.
public func haskellSKInit() {
  Color_initialise()
  Geometry_initialise()
  Node_initialise()
  Texture_initialise()
}
