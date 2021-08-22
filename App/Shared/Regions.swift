//
//  Regions.swift
//  GeocodeTest
//
//  Created by Sean Hess on 8/22/21.
//

import Foundation
import FlexibleGeohash
import Photos

struct Region {
  var name: String
  var latitude: CLLocationDegrees;
  var longitude: CLLocationDegrees;
  var countryCode: String;
  var provinceCode: String;
  var countryName: String;
  var provinceName: String?;
}



enum RegionError: Error {
  case noDatabaseFile
  case noDatabaseLoad
  case noRegions
  case noRegionParse(line:String)
}

struct NearestRegion {
  let region:Region
  let distance:Double;
}

typealias Hash = String

// Loads the regions database, only do this once!
class Regions {
  
    
  // For 46,000 photos on my 2017 macbook pro
  // 4 = 12 seconds, 40km precision
  // 5 = 22 seconds, 5km precision
  // 6 = 45 seconds, 1km precision, 2600 hashes
  let PRECISION = 5
  
  let all: [Region]
  
  init(regions:[Region]) {
    all = regions
  }
  
  func geohash(coord:CLLocationCoordinate2D) -> Geohash {
    return Geohash(coordinate:coord, precision: PRECISION)
  }
  
  // TODO - try sorting these into buckets. Has anyone else come up with a solution?
  func findNearest(coord:CLLocationCoordinate2D) -> Region {
    
    // runtime error if you haven't loaded regions yet. Should never happen
    let firstRegion = self.all[0]
    
    // start with the first
    var nearest:NearestRegion = NearestRegion(region: firstRegion, distance: self.distance(coord: coord, region: firstRegion))
    
    for region in self.all {
      let dist = self.distance(coord: coord, region: region)
      
      if (dist < nearest.distance) {
        nearest = NearestRegion(region: region, distance: dist)
      }
    }
    
    return nearest.region
  }
  
  func distance(coord:CLLocationCoordinate2D, region:Region) -> Double {
    return sqrt(pow(coord.latitude - region.latitude, 2) + pow(coord.longitude - region.longitude, 2))
  }
  
  class func loadFromDatabase() throws -> Regions {
    let rs = try Regions.loadDatabase()
    return Regions(regions: rs)
  }
  
  class func loadDatabase() throws -> [Region] {
    
    guard let filepath = Bundle.main.path(forResource: "regions", ofType: "tsv") else {
      throw RegionError.noDatabaseFile
    }
    
    guard let text = try? String(contentsOfFile: filepath) else {
      throw RegionError.noDatabaseLoad
    }
    
    let lines = text.components(separatedBy: "\n")
    return try lines.filter({$0 != ""}).map({ try toRegion(line: $0)})
  }
  
  
  class func toRegion(line:String) throws -> Region {
    let cols = line.components(separatedBy: "\t")
    
    guard (cols.count >= 6) else {
      throw RegionError.noRegionParse(line: line)
    }
    
//    let geoid = cols[0]
    let lat = CLLocationDegrees(cols[1])!
    let lng = CLLocationDegrees(cols[2])!
    let name = cols[3]
    let cc = cols[4]
    let pc = cols[5]
    let cn = cols[6]
    let pn = cols[7]
    
    return Region(name: name, latitude: lat, longitude: lng, countryCode: cc, provinceCode: pc, countryName: cn, provinceName: pn)
  }
}

