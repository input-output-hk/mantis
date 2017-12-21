package io.iohk.ethereum.ledger

import io.iohk.ethereum.utils.MonetaryPolicyConfig
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers}

class BlockRewardCalculatorSpec extends FlatSpec with Matchers with PropertyChecks {

  "BlockRewardCalculator" should "correctly calculate block and ommer rewards" in {
    val standardMP = MonetaryPolicyConfig(5000000, 0.2, 5000000000000000000L)

    val testMP = MonetaryPolicyConfig(10, 0.5, 5000000)

    val lowEraDurationMP = MonetaryPolicyConfig(3, 0.2, 5000000000000000000L)

    val table = Table[MonetaryPolicyConfig, BigInt, List[BigInt], BigInt, List[BigInt]](
      ("config", "blockNumber", "ommersNumbers", "expectedBlockReward", "expectedOmmersRewards"),
      (standardMP, 1, Nil, 5000000000000000000L, Nil),
      (standardMP, 1000000, List(999999), 5156250000000000000L, List(4375000000000000000L)),
      (standardMP, 5000000, List(4999998, 4999997), 5312500000000000000L, List(3750000000000000000L, 3125000000000000000L)),
      (standardMP, 5000000, Nil, 5000000000000000000L, Nil),
      (standardMP, 5000001, Nil, 4000000000000000000L, Nil),
      (standardMP, 7000000, List(6999999), 4125000000000000000L, List(125000000000000000L)),
      (standardMP, 10000000, List(9999998, 9999997), 4250000000000000000L, List(125000000000000000L, 125000000000000000L)),
      (standardMP, 20000000, List(19999998, 19999997), 2720000000000000000L, List(80000000000000000L, 80000000000000000L)),
      (standardMP, 20000001, List(19999998, 19999997), 2176000000000000000L, List(64000000000000000L, 64000000000000000L)),
      // era #193 is the last one where rewards for miners are non-zero
      (standardMP, 965000000, List(964999999, 964999999), 1, List(0, 0)),
      // era #194 - no rewards
      (standardMP, 965000001, List(964999999, 964999999), 0, List(0, 0)),
      (testMP, 10, List(9, 8), 5312500, List(4375000, 3750000)),
      (testMP, 11, List(9, 8), 2656250, List(78125, 78125)),
      (testMP, 20, Nil, 2500000, Nil),
      (testMP, 21, List(20), 1289062, List(39062)),

      //Era 21, which causes exponentiation vs loop error rounding error (See https://github.com/paritytech/parity/issues/6523)
      (lowEraDurationMP, 66, Nil, BigInt("46116860184273879"), Nil),

      //Causes ommer count multiplication rounding error, when calculating the reward given to the miner for including 2 ommers
      (lowEraDurationMP, 78, List(77, 77), BigInt("20070057552195990"), List(BigInt("590295810358705"), BigInt("590295810358705")))
    )

    forAll(table) { (config, blockNumber, ommersNumbers, expectedBlockReward, expectedOmmersRewards) =>
      val calculator = new BlockRewardCalculator(config)

      val blockReward = calculator.calcBlockMinerReward(blockNumber, ommersNumbers.size)
      val ommersRewards = ommersNumbers.map(calculator.calcOmmerMinerReward(blockNumber, _))

      blockReward shouldEqual expectedBlockReward
      ommersRewards shouldEqual expectedOmmersRewards
    }
  }

  it should "be compliant with ECIP1039 for block reward including zero, one or two, uncles until 50 era" in {

    val standardEraDuration = 5000000

    val standardMP = MonetaryPolicyConfig(5000000, 0.2, 5000000000000000000L)

    val ecip1039table = Table[MonetaryPolicyConfig, BigInt, BigInt, BigInt, BigInt](
      ("config", "blockNumber", "expectedBlockReward", "expectedWinnerOneUncleReward", "expectedWinnerTwoUnclesReward"),
      (standardMP,     standardEraDuration + 1, 4000000000000000000L, 4125000000000000000L, 4250000000000000000L),
      (standardMP, 2 * standardEraDuration + 1, 3200000000000000000L, 3300000000000000000L, 3400000000000000000L),
      (standardMP, 3 * standardEraDuration + 1, 2560000000000000000L, 2640000000000000000L, 2720000000000000000L),
      (standardMP, 4 * standardEraDuration + 1, 2048000000000000000L, 2112000000000000000L, 2176000000000000000L),
      (standardMP, 5 * standardEraDuration + 1, 1638400000000000000L, 1689600000000000000L, 1740800000000000000L),
      (standardMP, 6 * standardEraDuration + 1, 1310720000000000000L, 1351680000000000000L, 1392640000000000000L),
      (standardMP, 7 * standardEraDuration + 1, 1048576000000000000L, 1081344000000000000L, 1114112000000000000L),
      (standardMP, 8 * standardEraDuration + 1, 838860800000000000L, 865075200000000000L, 891289600000000000L),
      (standardMP, 9 * standardEraDuration + 1, 671088640000000000L, 692060160000000000L, 713031680000000000L),
      (standardMP, 10* standardEraDuration + 1, 536870912000000000L, 553648128000000000L, 570425344000000000L),
      (standardMP, 11* standardEraDuration + 1, 429496729600000000L, 442918502400000000L, 456340275200000000L),
      (standardMP, 12* standardEraDuration + 1, 343597383680000000L, 354334801920000000L, 365072220160000000L),
      (standardMP, 13* standardEraDuration + 1, 274877906944000000L, 283467841536000000L, 292057776128000000L),
      (standardMP, 14* standardEraDuration + 1, 219902325555200000L, 226774273228800000L, 233646220902400000L),
      (standardMP, 15* standardEraDuration + 1, 175921860444160000L, 181419418583040000L, 186916976721920000L),
      (standardMP, 16* standardEraDuration + 1, 140737488355328000L, 145135534866432000L, 149533581377536000L),
      (standardMP, 17* standardEraDuration + 1, 112589990684262400L, 116108427893145600L, 119626865102028800L),
      (standardMP, 18* standardEraDuration + 1, 90071992547409920L, 92886742314516480L, 95701492081623040L),
      (standardMP, 19* standardEraDuration + 1, 72057594037927936L, 74309393851613184L, 76561193665298432L),
      (standardMP, 20* standardEraDuration + 1, 57646075230342348L, 59447515081290546L, 61248954932238744L),
      (standardMP, 21* standardEraDuration + 1, 46116860184273879L, 47558012065032437L, 48999163945790995L),
      (standardMP, 22* standardEraDuration + 1, 36893488147419103L, 38046409652025949L, 39199331156632795L),
      (standardMP, 23* standardEraDuration + 1, 29514790517935282L, 30437127721620759L, 31359464925306236L),
      (standardMP, 24* standardEraDuration + 1, 23611832414348226L, 24349702177296608L, 25087571940244990L),
      (standardMP, 25* standardEraDuration + 1, 18889465931478580L, 19479761741837285L, 20070057552195990L),
      (standardMP, 26* standardEraDuration + 1, 15111572745182864L, 15583809393469828L, 16056046041756792L),
      (standardMP, 27* standardEraDuration + 1, 12089258196146291L, 12467047514775862L, 12844836833405433L),
      (standardMP, 28* standardEraDuration + 1, 9671406556917033L, 9973638011820690L, 10275869466724347L),
      (standardMP, 29* standardEraDuration + 1, 7737125245533626L, 7978910409456551L, 8220695573379476L),
      (standardMP, 30* standardEraDuration + 1, 6189700196426901L, 6383128327565241L, 6576556458703581L),
      (standardMP, 31* standardEraDuration + 1, 4951760157141521L, 5106502662052193L, 5261245166962865L),
      (standardMP, 32* standardEraDuration + 1, 3961408125713216L, 4085202129641754L, 4208996133570292L),
      (standardMP, 33* standardEraDuration + 1, 3169126500570573L, 3268161703713403L, 3367196906856233L),
      (standardMP, 34* standardEraDuration + 1, 2535301200456458L, 2614529362970722L, 2693757525484986L),
      (standardMP, 35* standardEraDuration + 1, 2028240960365167L, 2091623490376578L, 2155006020387989L),
      (standardMP, 36* standardEraDuration + 1, 1622592768292133L, 1673298792301262L, 1724004816310391L),
      (standardMP, 37* standardEraDuration + 1, 1298074214633706L, 1338639033841009L, 1379203853048312L),
      (standardMP, 38* standardEraDuration + 1, 1038459371706965L, 1070911227072807L, 1103363082438649L),
      (standardMP, 39* standardEraDuration + 1, 830767497365572L, 856728981658246L, 882690465950920L),
      (standardMP, 40* standardEraDuration + 1, 664613997892457L, 685383185326596L, 706152372760735L),
      (standardMP, 41* standardEraDuration + 1, 531691198313966L, 548306548261277L, 564921898208588L),
      (standardMP, 42* standardEraDuration + 1, 425352958651173L, 438645238609022L, 451937518566871L),
      (standardMP, 43* standardEraDuration + 1, 340282366920938L, 350916190887217L, 361550014853496L),
      (standardMP, 44* standardEraDuration + 1, 272225893536750L, 280732952709773L, 289240011882796L),
      (standardMP, 45* standardEraDuration + 1, 217780714829400L, 224586362167818L, 231392009506236L),
      (standardMP, 46* standardEraDuration + 1, 174224571863520L, 179669089734255L, 185113607604990L),
      (standardMP, 47* standardEraDuration + 1, 139379657490816L, 143735271787404L, 148090886083992L),
      (standardMP, 48* standardEraDuration + 1, 111503725992653L, 114988217429923L, 118472708867193L),
      (standardMP, 49* standardEraDuration + 1, 89202980794122L, 91990573943938L, 94778167093754L)
    )

    forAll(ecip1039table) { (config, blockNumber, expectedBlockReward, expectedWinnerOneUncleReward, expectedWinnerTwoUnclesReward) =>
      val calculator = new BlockRewardCalculator(config)

      val blockReward = calculator.calcBlockMinerReward(blockNumber, 0)
      val winnerOneUncleReward =calculator.calcBlockMinerReward(blockNumber, 1)
      val winnerTwoUnclesReward =calculator.calcBlockMinerReward(blockNumber, 2)

      blockReward shouldEqual expectedBlockReward
      winnerOneUncleReward shouldEqual expectedWinnerOneUncleReward
      winnerTwoUnclesReward shouldEqual expectedWinnerTwoUnclesReward
    }
  }

  it should "be compliant with ECIP1039 for block reward including two uncles until 200 era" in {

    val standardEraDuration = 5000000

    val standardMP = MonetaryPolicyConfig(5000000, 0.2, 5000000000000000000L)

    val ecip1039table = Table[MonetaryPolicyConfig, BigInt, BigInt](
      ("config", "blockNumber", "expectedWinnerTwoUnclesReward"),
      (standardMP,    standardEraDuration + 1, 4250000000000000000L),
      (standardMP, 2* standardEraDuration + 1, 3400000000000000000L),
      (standardMP, 3* standardEraDuration + 1, 2720000000000000000L),
      (standardMP, 4* standardEraDuration + 1, 2176000000000000000L),
      (standardMP, 5* standardEraDuration + 1, 1740800000000000000L),
      (standardMP, 6* standardEraDuration + 1, 1392640000000000000L),
      (standardMP, 7* standardEraDuration + 1, 1114112000000000000L),
      (standardMP, 8* standardEraDuration + 1, 891289600000000000L),
      (standardMP, 9* standardEraDuration + 1, 713031680000000000L),
      (standardMP, 10* standardEraDuration + 1, 570425344000000000L),
      (standardMP, 11* standardEraDuration + 1, 456340275200000000L),
      (standardMP, 12* standardEraDuration + 1, 365072220160000000L),
      (standardMP, 13* standardEraDuration + 1, 292057776128000000L),
      (standardMP, 14* standardEraDuration + 1, 233646220902400000L),
      (standardMP, 15* standardEraDuration + 1, 186916976721920000L),
      (standardMP, 16* standardEraDuration + 1, 149533581377536000L),
      (standardMP, 17* standardEraDuration + 1, 119626865102028800L),
      (standardMP, 18* standardEraDuration + 1, 95701492081623040L),
      (standardMP, 19* standardEraDuration + 1, 76561193665298432L),
      (standardMP, 20* standardEraDuration + 1, 61248954932238744L),
      (standardMP, 21* standardEraDuration + 1, 48999163945790995L),
      (standardMP, 22* standardEraDuration + 1, 39199331156632795L),
      (standardMP, 23* standardEraDuration + 1, 31359464925306236L),
      (standardMP, 24* standardEraDuration + 1, 25087571940244990L),
      (standardMP, 25* standardEraDuration + 1, 20070057552195990L),
      (standardMP, 26* standardEraDuration + 1, 16056046041756792L),
      (standardMP, 27* standardEraDuration + 1, 12844836833405433L),
      (standardMP, 28* standardEraDuration + 1, 10275869466724347L),
      (standardMP, 29* standardEraDuration + 1, 8220695573379476L),
      (standardMP, 30* standardEraDuration + 1, 6576556458703581L),
      (standardMP, 31* standardEraDuration + 1, 5261245166962865L),
      (standardMP, 32* standardEraDuration + 1, 4208996133570292L),
      (standardMP, 33* standardEraDuration + 1, 3367196906856233L),
      (standardMP, 34* standardEraDuration + 1, 2693757525484986L),
      (standardMP, 35* standardEraDuration + 1, 2155006020387989L),
      (standardMP, 36* standardEraDuration + 1, 1724004816310391L),
      (standardMP, 37* standardEraDuration + 1, 1379203853048312L),
      (standardMP, 38* standardEraDuration + 1, 1103363082438649L),
      (standardMP, 39* standardEraDuration + 1, 882690465950920L),
      (standardMP, 40* standardEraDuration + 1, 706152372760735L),
      (standardMP, 41* standardEraDuration + 1, 564921898208588L),
      (standardMP, 42* standardEraDuration + 1, 451937518566871L),
      (standardMP, 43* standardEraDuration + 1, 361550014853496L),
      (standardMP, 44* standardEraDuration + 1, 289240011882796L),
      (standardMP, 45* standardEraDuration + 1, 231392009506236L),
      (standardMP, 46* standardEraDuration + 1, 185113607604990L),
      (standardMP, 47* standardEraDuration + 1, 148090886083992L),
      (standardMP, 48* standardEraDuration + 1, 118472708867193L),
      (standardMP, 49* standardEraDuration + 1, 94778167093754L),
      (standardMP, 50* standardEraDuration + 1, 75822533675003L),
      (standardMP, 51* standardEraDuration + 1, 60658026940002L),
      (standardMP, 52* standardEraDuration + 1, 48526421552000L),
      (standardMP, 53* standardEraDuration + 1, 38821137241600L),
      (standardMP, 54* standardEraDuration + 1, 31056909793280L),
      (standardMP, 55* standardEraDuration + 1, 24845527834624L),
      (standardMP, 56* standardEraDuration + 1, 19876422267699L),
      (standardMP, 57* standardEraDuration + 1, 15901137814158L),
      (standardMP, 58* standardEraDuration + 1, 12720910251326L),
      (standardMP, 59* standardEraDuration + 1, 10176728201061L),
      (standardMP, 60* standardEraDuration + 1, 8141382560849L),
      (standardMP, 61* standardEraDuration + 1, 6513106048679L),
      (standardMP, 62* standardEraDuration + 1, 5210484838942L),
      (standardMP, 63* standardEraDuration + 1, 4168387871154L),
      (standardMP, 64* standardEraDuration + 1, 3334710296923L),
      (standardMP, 65* standardEraDuration + 1, 2667768237538L),
      (standardMP, 66* standardEraDuration + 1, 2134214590029L),
      (standardMP, 67* standardEraDuration + 1, 1707371672024L),
      (standardMP, 68* standardEraDuration + 1, 1365897337619L),
      (standardMP, 69* standardEraDuration + 1, 1092717870095L),
      (standardMP, 70* standardEraDuration + 1, 874174296076L),
      (standardMP, 71* standardEraDuration + 1, 699339436860L),
      (standardMP, 72* standardEraDuration + 1, 559471549488L),
      (standardMP, 73* standardEraDuration + 1, 447577239590L),
      (standardMP, 74* standardEraDuration + 1, 358061791671L),
      (standardMP, 75* standardEraDuration + 1, 286449433337L),
      (standardMP, 76* standardEraDuration + 1, 229159546669L),
      (standardMP, 77* standardEraDuration + 1, 183327637335L),
      (standardMP, 78* standardEraDuration + 1, 146662109867L),
      (standardMP, 79* standardEraDuration + 1, 117329687894L),
      (standardMP, 80* standardEraDuration + 1, 93863750314L),
      (standardMP, 81* standardEraDuration + 1, 75091000251L),
      (standardMP, 82* standardEraDuration + 1, 60072800200L),
      (standardMP, 83* standardEraDuration + 1, 48058240160L),
      (standardMP, 84* standardEraDuration + 1, 38446592128L),
      (standardMP, 85* standardEraDuration + 1, 30757273703L),
      (standardMP, 86* standardEraDuration + 1, 24605818961L),
      (standardMP, 87* standardEraDuration + 1, 19684655169L),
      (standardMP, 88* standardEraDuration + 1, 15747724134L),
      (standardMP, 89* standardEraDuration + 1, 12598179307L),
      (standardMP, 90* standardEraDuration + 1, 10078543446L),
      (standardMP, 91* standardEraDuration + 1, 8062834756L),
      (standardMP, 92* standardEraDuration + 1, 6450267806L),
      (standardMP, 93* standardEraDuration + 1, 5160214244L),
      (standardMP, 94* standardEraDuration + 1, 4128171394L),
      (standardMP, 95* standardEraDuration + 1, 3302537115L),
      (standardMP, 96* standardEraDuration + 1, 2642029692L),
      (standardMP, 97* standardEraDuration + 1, 2113623753L),
      (standardMP, 98* standardEraDuration + 1, 1690899002L),
      (standardMP, 99* standardEraDuration + 1, 1352719201L),
      (standardMP, 100* standardEraDuration + 1, 1082175362L),
      (standardMP, 101* standardEraDuration + 1, 865740288L),
      (standardMP, 102* standardEraDuration + 1, 692592230L),
      (standardMP, 103* standardEraDuration + 1, 554073783L),
      (standardMP, 104* standardEraDuration + 1, 443259027L),
      (standardMP, 105* standardEraDuration + 1, 354607222L),
      (standardMP, 106* standardEraDuration + 1, 283685777L),
      (standardMP, 107* standardEraDuration + 1, 226948621L),
      (standardMP, 108* standardEraDuration + 1, 181558896L),
      (standardMP, 109* standardEraDuration + 1, 145247118L),
      (standardMP, 110* standardEraDuration + 1, 116197694L),
      (standardMP, 111* standardEraDuration + 1, 92958154L),
      (standardMP, 112* standardEraDuration + 1, 74366523L),
      (standardMP, 113* standardEraDuration + 1, 59493218L),
      (standardMP, 114* standardEraDuration + 1, 47594574L),
      (standardMP, 115* standardEraDuration + 1, 38075659L),
      (standardMP, 116* standardEraDuration + 1, 30460526L),
      (standardMP, 117* standardEraDuration + 1, 24368422L),
      (standardMP, 118* standardEraDuration + 1, 19494736L),
      (standardMP, 119* standardEraDuration + 1, 15595789L),
      (standardMP, 120* standardEraDuration + 1, 12476630L),
      (standardMP, 121* standardEraDuration + 1, 9981304L),
      (standardMP, 122* standardEraDuration + 1, 7985044L),
      (standardMP, 123* standardEraDuration + 1, 6388035L),
      (standardMP, 124* standardEraDuration + 1, 5110427L),
      (standardMP, 125* standardEraDuration + 1, 4088342L),
      (standardMP, 126* standardEraDuration + 1, 3270673L),
      (standardMP, 127* standardEraDuration + 1, 2616539L),
      (standardMP, 128* standardEraDuration + 1, 2093230L),
      (standardMP, 129* standardEraDuration + 1, 1674584L),
      (standardMP, 130* standardEraDuration + 1, 1339668L),
      (standardMP, 131* standardEraDuration + 1, 1071733L),
      (standardMP, 132* standardEraDuration + 1, 857387L),
      (standardMP, 133* standardEraDuration + 1, 685908L),
      (standardMP, 134* standardEraDuration + 1, 548727L),
      (standardMP, 135* standardEraDuration + 1, 438981L),
      (standardMP, 136* standardEraDuration + 1, 351183L),
      (standardMP, 137* standardEraDuration + 1, 280948L),
      (standardMP, 138* standardEraDuration + 1, 224757L),
      (standardMP, 139* standardEraDuration + 1, 179806L),
      (standardMP, 140* standardEraDuration + 1, 143844L),
      (standardMP, 141* standardEraDuration + 1, 115075L),
      (standardMP, 142* standardEraDuration + 1, 92059L),
      (standardMP, 143* standardEraDuration + 1, 73648L),
      (standardMP, 144* standardEraDuration + 1, 58917L),
      (standardMP, 145* standardEraDuration + 1, 47134L),
      (standardMP, 146* standardEraDuration + 1, 37708L),
      (standardMP, 147* standardEraDuration + 1, 30166L),
      (standardMP, 148* standardEraDuration + 1, 24131L),
      (standardMP, 149* standardEraDuration + 1, 19304L),
      (standardMP, 150* standardEraDuration + 1, 15444L),
      (standardMP, 151* standardEraDuration + 1, 12355L),
      (standardMP, 152* standardEraDuration + 1, 9883L),
      (standardMP, 153* standardEraDuration + 1, 7906L),
      (standardMP, 154* standardEraDuration + 1, 6326L),
      (standardMP, 155* standardEraDuration + 1, 5059L),
      (standardMP, 156* standardEraDuration + 1, 4048L),
      (standardMP, 157* standardEraDuration + 1, 3238L),
      (standardMP, 158* standardEraDuration + 1, 2590L),
      (standardMP, 159* standardEraDuration + 1, 2071L),
      (standardMP, 160* standardEraDuration + 1, 1656L),
      (standardMP, 161* standardEraDuration + 1, 1326L),
      (standardMP, 162* standardEraDuration + 1, 1060L),
      (standardMP, 163* standardEraDuration + 1, 847L),
      (standardMP, 164* standardEraDuration + 1, 677L),
      (standardMP, 165* standardEraDuration + 1, 541L),
      (standardMP, 166* standardEraDuration + 1, 433L),
      (standardMP, 167* standardEraDuration + 1, 347L),
      (standardMP, 168* standardEraDuration + 1, 277L),
      (standardMP, 169* standardEraDuration + 1, 221L),
      (standardMP, 170* standardEraDuration + 1, 177L),
      (standardMP, 171* standardEraDuration + 1, 142L),
      (standardMP, 172* standardEraDuration + 1, 113L),
      (standardMP, 173* standardEraDuration + 1, 89L),
      (standardMP, 174* standardEraDuration + 1, 72L),
      (standardMP, 175* standardEraDuration + 1, 56L),
      (standardMP, 176* standardEraDuration + 1, 45L),
      (standardMP, 177* standardEraDuration + 1, 37L),
      (standardMP, 178* standardEraDuration + 1, 28L),
      (standardMP, 179* standardEraDuration + 1, 22L),
      (standardMP, 180* standardEraDuration + 1, 17L),
      (standardMP, 181* standardEraDuration + 1, 14L),
      (standardMP, 182* standardEraDuration + 1, 11L),
      (standardMP, 183* standardEraDuration + 1, 9L),
      (standardMP, 184* standardEraDuration + 1, 7L),
      (standardMP, 185* standardEraDuration + 1, 5L),
      (standardMP, 186* standardEraDuration + 1, 4L),
      (standardMP, 187* standardEraDuration + 1, 3L),
      (standardMP, 188* standardEraDuration + 1, 3L),
      (standardMP, 189* standardEraDuration + 1, 2L),
      (standardMP, 190* standardEraDuration + 1, 1L),
      (standardMP, 191* standardEraDuration + 1, 1L),
      (standardMP, 192* standardEraDuration + 1, 1L),
      (standardMP, 193* standardEraDuration + 1, 0L),
      (standardMP, 194* standardEraDuration + 1, 0L),
      (standardMP, 195* standardEraDuration + 1, 0L),
      (standardMP, 196* standardEraDuration + 1, 0L),
      (standardMP, 197* standardEraDuration + 1, 0L),
      (standardMP, 198* standardEraDuration + 1, 0L),
      (standardMP, 199* standardEraDuration + 1, 0L)

    )

    forAll(ecip1039table) { (config, blockNumber, expectedWinnerTwoUnclesReward) =>
      val calculator = new BlockRewardCalculator(config)

      val winnerTwoUnclesReward =calculator.calcBlockMinerReward(blockNumber, 2)

      winnerTwoUnclesReward shouldEqual expectedWinnerTwoUnclesReward
    }
  }
}
