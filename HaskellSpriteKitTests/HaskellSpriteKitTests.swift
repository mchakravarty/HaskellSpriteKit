//
//  HaskellSpriteKitTests.swift
//  HaskellSpriteKitTests
//
//  Created by Manuel M T Chakravarty on 24/10/2014.
//  Copyright (c) 2014 Manuel M T Chakravarty. All rights reserved.
//

import Cocoa
import XCTest

import HaskellSpriteKit
import GHCKit

class HaskellSpriteKitTests: XCTestCase {

//  var ghcInstance: GHCInstance

  override func setUp() {
    super.setUp()
//    ghcInstance = GHCInstance(diagnosticsHandler: {severity, filename, line, column, lines, endColumn, message in })
  }
  
  override func tearDown() {
//    ghcInstance = nil
    super.tearDown()
  }
  
  func testExample() {
    let ghcInstance = GHCInstance(diagnosticsHandler: {severity, filename, line, column, lines, endColumn, message in },
                                  interactiveWorkingDirectory: "/tmp",
                                  stdoutForwarder: { _text in },
                                  stderrForwarder: { _text in })
    let result = ghcInstance.evalExprFromString("42", source: "<test>", line: 1)
    XCTAssert(result.count == 2, "Pass")
  }
    
//    func testPerformanceExample() {
        // This is an example of a performance test case.
//        self.measureBlock() {
            // Put the code you want to measure the time of here.
//        }
//    }

}
