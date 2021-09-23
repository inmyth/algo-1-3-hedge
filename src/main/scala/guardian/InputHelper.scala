package guardian

import com.hsoft.hmm.api.automaton.Automaton
import com.ingalys.imc.BuySell
import com.ingalys.imc.depth.Depth
import guardian.Error.UnknownError

import scala.annotation.tailrec

object InputHelper {

//  @tailrec
//  final def accumulateDepthVolume(mmPrice: Double, buySell: Int, depth: Depth, rank: Int, acc: Long): Long ={
//    val x = if(buySell == BuySell.BUY) depth.getBuy(rank) else depth.getSell(rank)
//    val v = acc + x.getQuantityL
//    if (x.getPrice == mmPrice){
//      v
//    } else{
//      accumulateDepthVolume(mmPrice, buySell, depth, rank + 1, v)
//    }
//  }
//
}
