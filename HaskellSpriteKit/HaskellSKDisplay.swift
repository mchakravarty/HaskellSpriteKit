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


//// Must be called, for now, before using any HaskellSpriteKit functions.
//public func haskellSKInit() {
//  Color_initialise()
//  Geometry_initialise()
//  Node_initialise()
//  Texture_initialise()
//}
// Doesn't make any sense as it is the wrong copy of the dylib...


/// If the argument is a SpriteKit object, wrap it into a view for display.
///
public func spriteKitView(obj: AnyObject) -> NSView? {
  if let node = obj as? SKNode {

    // FIXME: we need to deal with very small and very big scenes
    let sceneSize = node.calculateAccumulatedFrame().size
    var scene     = SKScene(size: sceneSize)
    var sceneView = SKView(frame: CGRect(origin: CGPointZero, size: sceneSize))
    scene.scaleMode = .AspectFill
    sceneView.presentScene(scene)
    return sceneView

  } else { return nil }
}