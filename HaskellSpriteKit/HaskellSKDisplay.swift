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


let kPreferenceSpriteKitLogLevel = "SpriteKitLogLevel"  // Must coincide with the key in 'HfM.Constants'.


/// If the argument is a SpriteKit object, wrap it into a scene for display.
///
public func spriteKitView(obj: AnyObject) -> SKScene? {
  let logLevel = NSUserDefaults.standardUserDefaults().integerForKey(kPreferenceSpriteKitLogLevel)

  if let scene = obj as? SKScene { logNodeName(logLevel, node: scene); return scene }
  else if let node = obj as? SKNode {

    logNodeName(logLevel, node: node)

    let sceneFrame   = node.calculateAccumulatedFrame()
    let scene        = SKScene(size: sceneFrame.size)
    node.position.x += -sceneFrame.origin.x
    node.position.y += -sceneFrame.origin.y
    scene.scaleMode  = .AspectFill
    scene.addChild(node)
    return scene

  } else if let texture = obj as? SKTexture {

    let sceneSize    = texture.size()
    let scene        = SKScene(size: sceneSize)
    let node         = SKSpriteNode(texture: texture)
    node.position.x += sceneSize.width  / 2
    node.position.y += sceneSize.height / 2
    scene.scaleMode  = .AspectFill
    scene.addChild(node)
    return scene

  } else if let colour = obj as? SKColor {

    let size         = CGSize(width: 30, height: 15)
    let scene        = SKScene(size: size)
    let node         = SKSpriteNode(color: colour, size: size)
    node.position.x += size.width  / 2
    node.position.y += size.height / 2
    scene.scaleMode  = .AspectFill
    scene.addChild(node)
    return scene
    
  } else { return nil }
}

private func logNodeName(logLevel: Int, node: SKNode) {
  if logLevel > 0 {

    let className = node.className
    let name      = node.name == nil ? "" : " named \"" + node.name! + "\""
    NSLog("[HaskellSKDisplay] view for \(className)\(name)")

  }
}