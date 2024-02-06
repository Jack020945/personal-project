test = {
  'name': 'Extra Credit',
  'points': 1,
  'suites': [
    {
      'cases': [
        {
          'code': r"""
          >>> _, code = lab12_extra_credit()
          >>> hashlib.md5(str.encode(code)).hexdigest()[:6] in ['564a0e', '45fba1', 'c3692b', 'e5bb20', 'a63c4b', '1d4245', '3aab22', 'a26304', 'ead870', '9bc9fc', '9b2bbc', '2acac5', 'a827f3', 'b200b0', '3d40bd', 'd70784', '61ce58', '13ed08', '8f7229', '63f523', '1afd47', '9d0718', 'bd620d', 'e0410c', '91d6ae', '3f4b1b', '3a2608', 'ff9170', 'e756a9', '5b0e5e', '5b5547', 'baf89e', '84e07b', '63f720', '35c2d2', '3cd9f8', '6d1158', '6b11a8', 'a59e4a', 'bf0615', '1c5802', 'e8e931', 'e3547a', 'a2813d', '46992f', '28fb52', 'ad07ae', 'e5db59', '9d79b0', '6ed364', '72f25f', '411f84', 'a2528d', '5c36bb', '36cdaf', 'a77a68', 'b77398', '340521', '2659ac', 'f0106a', 'd30df8', '2846a1', 'f71bc4', 'd918c2', 'e44191', '0baf6f', 'd961bd', '1f79a2', '653e42', '4fd4e2', 'd83ebd', 'ad3de9', '7d5d0e', '23a84b', 'f6b9b6', '9ed39d', '53feab', '6043c8', 'c7514d', 'e155bd', '3083cc', 'ab2ad8', '6de683', '4af971', 'c3f294', '4f2b21', 'f29e2b', 'd14c57', 'ae47bc', 'da6959', '304a3d', '9915b6', '737e54', '90a592', '378b17', '555f10', '86e801', 'fbfb4d', 'e12365', 'c2e71d', '7352e6', 'a70010', '0fa9b3', '9a47e0', '6fcd53', '906887', '9bd3a0', 'fdf8eb', '804284', '875418', 'eb7913', '9f8362', 'cacc37', 'c1c5d1', '0a4a67', 'a38de9', '96805f', 'ab6268', 'f84b86', 'a0d7e1', 'a5d681', '7cd809', '3719fe', '001f04', 'f5c38a', 'a4c8b2', '4f7179', 'bec1da', 'b5abaa', 'fc420c', '07ae6c', '085f27', '7c3f93', '18ef6a', '2344c3', '620cef', '9ca840', '1709ee', 'ad35ae', 'b4b2f8', '7b7e41', 'b5fbf8', '77607c', 'b73d64', 'f64f1a', 'c457f3', '02dcf8', 'a4904b', '8eb67a', '81db1a', '736151', '99e0ec', '272d07', '6a8e1d', 'c99a36', 'c702bd', 'e7252a', '0ff820', '544b2a', '63a992', '325420', '545a89', '8fe3ea', 'e61908', '35f010', '61380f', '660393', '7e3220', 'c5d875', '48b341', 'f0bb3a', 'b3ac95', 'c992b3', 'ed00f2', '7e1ea7', 'b2dae3', 'cce11f', '23aacb', '9a75a6', '0e6775', '8678f7', '446d0c', '58f8be', 'a33e90', '45ae04', 'f388e6', 'eb2ced', '560993', '2dc17a', '761cad', 'a42d1b', '701aad', '6b04f5', 'a4406d', '841bdd', '2611fd', '0d370b', 'e3c945', 'd80261', '61d85a', 'c9e201', '5dd283', '2a9d58', 'b57017', '65ad52', 'db3704', 'aa984b', 'd197af', '2bb8db', '137fa4', '88730e', '225714', '4f1174', '14ed5f', 'db9e1e', 'cd1f23', '09ee5f', 'cf2e20', '4e4035', 'e37212', 'dd2e73', 'be757a', '0e6419', '494094', '269eca', 'dba0ce', 'fdc11c', 'da0706', '9be5c4', 'ae9ce3', 'ccfa14', 'dc9529', 'e53df7', '8f7ae6', '6179e9', '4e6398', '5b663d', '6fabe7', '6298b5', '2fa39f', '331419', '8acef1', 'da5d58', 'fc4701', '31230d', '268130', 'e40ac0', 'c230ba', 'c743ba', '4e6f21', 'e6d771', 'cc12eb', 'a91bbe', 'cd4a76', '0e9f15', '3d2a74', '958af3', 'b6f571', '400b7f', '3e1b46', '4fe1fd', 'c81387', 'b1ca92', '7bd4dd', 'e485e3', 'd128ff', '61e36f', '40c54c', 'fff9e7', '13f0d7', '9e521a', '9a7a8f', 'dd802e', '7bab00', '87d764', 'b4cfab', 'dc0f06', 'd05a07', 'fbecfd', 'ffe3ae', '660381', '3da113', '87405f', '56cf36', 'cd125d', '5efd4e', '96a593', '48c4c2', '2d10ad', 'c0d70e', 'b7f020', '9e15f6', 'a9e01e', 'f24143', '376bc5', '267d00', '4c9327', '164c69', '4aaead', '50c423', '7058a5', 'f74a6c']
          True
          """,
          'hidden': False,
          'locked': False
        }
      ],
      'scored': True,
      'setup': r"""
      >>> from lab12 import *
      >>> import hashlib
      """,
      'teardown': '',
      'type': 'doctest'
    }
  ]
}
