//
//  FixShapeNode.swift
//  HaskellSpriteKit
//
//  Created by Manuel M T Chakravarty on 24/11/2015.
//  Copyright © 2015 Manuel M T Chakravarty. All rights reserved.
//

import Foundation
import SpriteKit

public extension SKNode {

  /// Replace all `SKShapeNode`s in the tree rooted at the current node by a copy to work around RDAR 23646973.
  ///
  /// This will lose all actions in `SKShapeNode`s.
  ///
  func fix() { children.forEach{ $0.fix() } }
}

public extension SKShapeNode {

  override func fix() {
    removeAllActions()
    let clone = copy() as! SKShapeNode
    if let parent = parent {
      let index = parent.children.indexOf(self) ?? 0
      removeFromParent()
      parent.insertChild(clone, atIndex: index)
    }
  }
}