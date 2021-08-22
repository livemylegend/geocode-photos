//
//  Photos.swift
//  GeocodeTest
//
//  Created by Sean Hess on 8/22/21.
//

import Foundation
import FlexibleGeohash
import Photos


enum PhotoRegion {
  case initialized
  case noLocation
  case geohashed(hash:Geohash)
  case located(region:Region)
}

class Photo: Identifiable {
  
  let id:String
  let image:PHAsset
  var region:PhotoRegion
  
  init(asset:PHAsset) {
    id = asset.localIdentifier
    image = asset
    region = PhotoRegion.initialized
  }
  
  func isLocated() -> Bool {
    switch region {
    case .located(_): return true
    case _: return false
    }
  }
}

class Photos {
  class func loadAll() -> PHFetchResult<PHAsset> {
    let fetchOptions = PHFetchOptions()
    fetchOptions.fetchLimit = 0
    return PHAsset.fetchAssets(with: .image, options: fetchOptions)
  }
  
  class func hashPhotos(regions:Regions, result:PHFetchResult<PHAsset>) -> PhotoHashes {
    let hashes = PhotoHashes()
    
    result.enumerateObjects({(asset, _, _) in
      let photo = Photo(asset:asset)
      
      guard let loc = asset.location else {
        hashes.addNoLocation(photo: photo)
        return
      }
      
      let hash = regions.geohash(coord: loc.coordinate)
      hashes.addHash(hash: hash, photo: photo)
    })
    
    return hashes
  }
  
  class func locateHashPhotos(regions:Regions, geohash:Geohash, photos:[Photo]) -> [Photo] {
    
    let coord = geohash.region().mk().center
    let region = regions.findNearest(coord: coord)

    for photo in photos {
      photo.region = .located(region: region)
    }
      
//    print("Found Hash:", geohash.hash(), photos.count, region.name, region.provinceName ?? "", region.countryName)
    
    return photos
  }
}



// Group the photos by geohash
class PhotoHashes {
  var none: [Photo] = []
  var geo: [Geohash:[Photo]] = [:]
  
  func addNoLocation(photo:Photo) {
    photo.region = .noLocation
    self.none.append(photo)
  }
  
  func addHash(hash:Geohash, photo:Photo) {
    var hashes:[Photo] = self.geo[hash] ?? []
    photo.region = .geohashed(hash: hash)
    hashes.append(photo)
    self.geo[hash] = hashes
  }
}


// So we can use Geohash as a dictionary key
extension Geohash: Hashable {
  public func hash(into hasher: inout Hasher) {
    hasher.combine(self.hash())
  }
  
  static public func == (lhs: Geohash, rhs: Geohash) -> Bool {
      return lhs.hash() == rhs.hash()
  }
}
