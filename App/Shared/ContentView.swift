//
//  ContentView.swift
//  Shared
//
//  Created by Sean Hess on 8/6/21.
//

import SwiftUI
import Photos
import MapKit
import FlexibleGeohash

enum Processing {
  case ready
  case started
  case loaded(n:Int)
  case hashed
  case processed(noLocated: Int, located:Int, tot:Int)
  case complete
}

struct ContentView: View {
    
  @State var status: Processing = .ready
  @State var counter: Int = 0
  @State var photos: [Photo] = []
  @State var hashes: PhotoHashes = PhotoHashes()
  
  var queue:OperationQueue = OperationQueue()
  
  var body: some View {
    VStack {
      Text(viewStatus(s: status))
      Button("Load Photos", action: loadPhotos)
      
      // This is provided just to prove the UI isn't locked.
      // You can click the button and see the view respond
      Button("Counter", action: incrementCounter)
      Text(String(counter))

    }.padding().frame(width: 500, height: 500, alignment: Alignment.center)
  }
  
  func incrementCounter() {
    self.counter += 1
  }
  
  func viewStatus(s:Processing) -> String {
    switch s {
    case .ready: return "ready"
    case .hashed: return "hashed: " + String(self.hashes.geo.keys.count)
    case .loaded(let tot): return "loaded: " + String(tot)
    case .started: return "started"
    case .processed(let noLocated, let located, let total):
      return "located: " + String(located) + " - noLocated: " + String(noLocated) + " - total: " + String(total)
    case .complete: return "complete"
    }
  }
  
  func loadPhotos() {
    status = .started
    
    // Run in the bacjkground
    DispatchQueue.global().async {
      do {
        
        let regions = try Regions.loadFromDatabase()
        
        let fetchResult = Photos.loadAll()
        let total = fetchResult.count
        self.status = .loaded(n: total)
        
        self.hashes = Photos.hashPhotos(regions: regions, result: fetchResult)
        self.status = .hashed
        

        // start with no location photos
        
        self.photos = hashes.none
        var located = 0
        let noLocated = hashes.none.count
        
        for (hash, photos) in self.hashes.geo {
          
          // Locate each hash group with an operation queue
          // allows us to stop / start / cancel and monitor them if needed
          queue.addOperation {
            
            let hashPhotos = Photos.locateHashPhotos(regions: regions, geohash: hash, photos: photos)
            
            self.photos = self.photos + hashPhotos
            located += hashPhotos.count
            
            // report progress back to the view
            if (noLocated + located == total) {
              self.status = .complete
            }
            else {
              self.status = .processed(noLocated: noLocated, located: located, tot: total)
            }
          }
        }
        
        self.status = .complete
        
      } catch {
        // TODO handle in user interface
        print("Photos Error!", error)
      }
    }
  }
}

struct ContentView_Previews: PreviewProvider {
  static var previews: some View {
    ContentView()
  }
}


