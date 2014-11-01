//
//  HaskellSKDisplay.swift
//  HaskellSpriteKit
//
//  Created by Manuel M T Chakravarty on 28/10/2014.
//  Copyright (c) 2014 Manuel M T Chakravarty. All rights reserved.
//

import Foundation
import Cocoa
import SpriteKit


/// If the argument is a SpriteKit object, wrap it into a scene for display.
///
public func spriteKitView(obj: AnyObject) -> SKScene? {
  if let scene = obj as? SKScene { return scene }
  else {
    if let node = obj as? SKNode {

      let sceneFrame = node.calculateAccumulatedFrame()
      var scene      = SKScene(size: sceneFrame.size)
      var sceneView  = SKView(frame: CGRect(origin: CGPointZero, size: sceneFrame.size))
      scene.scaleMode   = .ResizeFill
      scene.addChild(node)
      return scene

    } else { return nil }
  }
}
