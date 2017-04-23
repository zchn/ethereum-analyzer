{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module SpecCommon
  ( hexstring1
  , hexstring2
  , voteHexstring
  ) where

import Ethereum.Analyzer.Disasm

hexstring1 :: EvmHexString
hexstring1 =
  EvmHexString
    "6060604052600261010860005055604051611b51380380611b51833981016040528080518201919060200180519060200190919080519060200190919050505b805b83835b600060018351016001600050819055503373ffffffffffffffffffffffffffffffffffffffff16600260005060016101008110156100025790900160005b5081905550600161010260005060003373ffffffffffffffffffffffffffffffffffffffff16815260200190815260200160002060005081905550600090505b825181101561016e5782818151811015610002579060200190602002015173ffffffffffffffffffffffffffffffffffffffff166002600050826002016101008110156100025790900160005b508190555080600201610102600050600085848151811015610002579060200190602002015173ffffffffffffffffffffffffffffffffffffffff168152602001908152602001600020600050819055505b80600101905080506100c2565b816000600050819055505b505050806101056000508190555061018f6101ad565b610107600050819055505b505b505050611992806101bf6000396000f35b600062015180420490506101bc565b9056606060405236156100f8576000357c010000000000000000000000000000000000000000000000000000000090048063173825d9146101605780632f54bf6e146101785780634123cb6b146101a457806352375093146101c757806354fd4d50146101ea5780635c52c2f51461020d578063659010e71461021c5780637065cb481461023f578063746c917114610257578063797af6271461027a578063b20d30a9146102a6578063b61d27f6146102be578063b75c7dc614610307578063ba51a6df1461031f578063c2cf732614610337578063cbf0b0c01461036c578063f00d4b5d14610384578063f1736d86146103a5576100f8565b61015e5b600034111561015b577fe1fffcc4923d04b559f4d29a8bfc6cda04eb5b0d3c460751c2402c5c5cc9109c3334604051808373ffffffffffffffffffffffffffffffffffffffff1681526020018281526020019250505060405180910390a15b5b565b005b61017660048080359060200190919050506107c4565b005b61018e60048080359060200190919050506109a5565b6040518082815260200191505060405180910390f35b6101b16004805050610a91565b6040518082815260200191505060405180910390f35b6101d46004805050610b38565b6040518082815260200191505060405180910390f35b6101f76004805050610b42565b6040518082815260200191505060405180910390f35b61021a6004805050610adf565b005b6102296004805050610b2e565b6040518082815260200191505060405180910390f35b610255600480803590602001909190505061066e565b005b6102646004805050610a88565b6040518082815260200191505060405180910390f35b6102906004808035906020019091905050610f0e565b6040518082815260200191505060405180910390f35b6102bc6004808035906020019091905050610a9a565b005b6102f160048080359060200190919080359060200190919080359060200190820180359060200191909192905050610b9e565b6040518082815260200191505060405180910390f35b61031d60048080359060200190919050506103c8565b005b610335600480803590602001909190505061090f565b005b61035660048080359060200190919080359060200190919050506109e7565b6040518082815260200191505060405180910390f35b6103826004808035906020019091905050610b4c565b005b6103a360048080359060200190919080359060200190919050506104ca565b005b6103b26004805050610b24565b6040518082815260200191505060405180910390f35b60006000600061010260005060003373ffffffffffffffffffffffffffffffffffffffff168152602001908152602001600020600050549250600083141561040f576104c4565b8260020a91506101036000506000858152602001908152602001600020600050905060008282600101600050541611156104c3578060000160008181505480929190600101919050555081816001016000828282505403925050819055507fc7fb647e59b18047309aa15aad418e5d7ca96d173ad704f1031a2c3d7591734b3385604051808373ffffffffffffffffffffffffffffffffffffffff1681526020018281526020019250505060405180910390a15b5b50505050565b600060003643604051808484808284378201915050828152602001935050505060405180910390206104fb816112db565b1561066757610509836109a5565b156105145750610669565b61010260005060008573ffffffffffffffffffffffffffffffffffffffff16815260200190815260200160002060005054915060008214156105565750610669565b61055e611777565b8273ffffffffffffffffffffffffffffffffffffffff166002600050836101008110156100025790900160005b5081905550600061010260005060008673ffffffffffffffffffffffffffffffffffffffff168152602001908152602001600020600050819055508161010260005060008573ffffffffffffffffffffffffffffffffffffffff168152602001908152602001600020600050819055507fb532073b38c83145e3e5135377a08bf9aab55bc0fd7c1179cd4fb995d2a5159c8484604051808373ffffffffffffffffffffffffffffffffffffffff1681526020018273ffffffffffffffffffffffffffffffffffffffff1681526020019250505060405180910390a15b505b505050565b600036436040518084848082843782019150508281526020019350505050604051809103902061069d816112db565b156107bf576106ab826109a5565b156106b657506107c1565b6106be611777565b60fa6001600050541015156106d7576106d561153d565b505b60fa6001600050541015156106ec57506107c1565b60016000818150548092919060010191905055508173ffffffffffffffffffffffffffffffffffffffff1660026000506001600050546101008110156100025790900160005b508190555060016000505461010260005060008473ffffffffffffffffffffffffffffffffffffffff168152602001908152602001600020600050819055507f994a936646fe87ffe4f1e469d3d6aa417d6b855598397f323de5b449f765f0c382604051808273ffffffffffffffffffffffffffffffffffffffff16815260200191505060405180910390a15b505b50565b600060003643604051808484808284378201915050828152602001935050505060405180910390206107f5816112db565b156109095761010260005060008473ffffffffffffffffffffffffffffffffffffffff168152602001908152602001600020600050549150600082141561083c575061090b565b6001600160005054036000600050541115610857575061090b565b60006002600050836101008110156100025790900160005b5081905550600061010260005060008573ffffffffffffffffffffffffffffffffffffffff168152602001908152602001600020600050819055506108b2611777565b6108ba61153d565b507f58619076adf5bb0943d100ef88d52d7c3fd691b19d3a9071b555b651fbf418da83604051808273ffffffffffffffffffffffffffffffffffffffff16815260200191505060405180910390a15b505b5050565b600036436040518084848082843782019150508281526020019350505050604051809103902061093e816112db565b156109a05760016000505482111561095657506109a2565b81600060005081905550610968611777565b7facbdb084c721332ac59f9b8e392196c9eb0e4932862da8eb9beaf0dad4f550da826040518082815260200191505060405180910390a15b505b50565b6000600061010260005060008473ffffffffffffffffffffffffffffffffffffffff168152602001908152602001600020600050541190506109e2565b919050565b60006000600060006101036000506000878152602001908152602001600020600050925061010260005060008673ffffffffffffffffffffffffffffffffffffffff1681526020019081526020016000206000505491506000821415610a505760009350610a7f565b8160020a90506000818460010160005054161415610a755760009350610a7f56610a7e565b60019350610a7f565b5b50505092915050565b60006000505481565b60016000505481565b6000364360405180848480828437820191505082815260200193505050506040518091039020610ac9816112db565b15610ada5781610105600050819055505b505b50565b6000364360405180848480828437820191505082815260200193505050506040518091039020610b0e816112db565b15610b20576000610106600050819055505b505b565b6101056000505481565b6101066000505481565b6101076000505481565b6101086000505481565b6000364360405180848480828437820191505082815260200193505050506040518091039020610b7b816112db565b15610b99578173ffffffffffffffffffffffffffffffffffffffff16ff5b505b50565b6000610ba9336109a5565b15610f0557610bb7846116d7565b15610ca0577f92ca3a80853e6663fa31fa10b99225f18d4902939b4c53a9caae9043f6efd0043385878686604051808673ffffffffffffffffffffffffffffffffffffffff1681526020018581526020018473ffffffffffffffffffffffffffffffffffffffff1681526020018060200182810382528484828181526020019250808284378201915050965050505050505060405180910390a18473ffffffffffffffffffffffffffffffffffffffff168484846040518083838082843782019150509250505060006040518083038185876185025a03f1925050505060006001029050610f06565b600036436040518084848082843782019150508281526020019350505050604051809103902090508050610cd381610f0e565b158015610d3357506000610109600050600083815260200190815260200160002060005060000160009054906101000a900473ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16145b15610f045784610109600050600083815260200190815260200160002060005060000160006101000a81548173ffffffffffffffffffffffffffffffffffffffff0219169083021790555083610109600050600083815260200190815260200160002060005060010160005081905550828261010960005060008481526020019081526020016000206000506002016000509190828054600181600116156101000203166002900490600052602060002090601f016020900481019282601f10610e0857803560ff1916838001178555610e39565b82800160010185558215610e39579182015b82811115610e38578235826000505591602001919060010190610e1a565b5b509050610e649190610e46565b80821115610e605760008181506000905550600101610e46565b5090565b50507f1733cbb53659d713b79580f79f3f9ff215f78a7c7aa45890f3b89fc5cddfbf32813386888787604051808781526020018673ffffffffffffffffffffffffffffffffffffffff1681526020018581526020018473ffffffffffffffffffffffffffffffffffffffff168152602001806020018281038252848482818152602001925080828437820191505097505050505050505060405180910390a15b5b5b949350505050565b600081610f1a816112db565b156112d4576000610109600050600085815260200190815260200160002060005060000160009054906101000a900473ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff161415156112d357610109600050600084815260200190815260200160002060005060000160009054906101000a900473ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16610109600050600085815260200190815260200160002060005060010160005054610109600050600086815260200190815260200160002060005060020160005060405180828054600181600116156101000203166002900480156110765780601f1061104b57610100808354040283529160200191611076565b820191906000526020600020905b81548152906001019060200180831161105957829003601f168201915b505091505060006040518083038185876185025a03f192505050507fe7c957c06e9a662c1a6c77366179f5b702b97651dc28eee7d5bf1dff6e40bb4a3384610109600050600087815260200190815260200160002060005060010160005054610109600050600088815260200190815260200160002060005060000160009054906101000a900473ffffffffffffffffffffffffffffffffffffffff166101096000506000898152602001908152602001600020600050600201600050604051808673ffffffffffffffffffffffffffffffffffffffff1681526020018581526020018481526020018373ffffffffffffffffffffffffffffffffffffffff168152602001806020018281038252838181546001816001161561010002031660029004815260200191508054600181600116156101000203166002900480156112005780601f106111d557610100808354040283529160200191611200565b820191906000526020600020905b8154815290600101906020018083116111e357829003601f168201915b5050965050505050505060405180910390a1610109600050600084815260200190815260200160002060006000820160006101000a81549073ffffffffffffffffffffffffffffffffffffffff0219169055600182016000506000905560028201600050805460018160011615610100020316600290046000825580601f1061128957506112c6565b601f0160209004906000526020600020908101906112c591906112a7565b808211156112c157600081815060009055506001016112a7565b5090565b5b50505060019150506112d6565b5b505b919050565b600060006000600061010260005060003373ffffffffffffffffffffffffffffffffffffffff168152602001908152602001600020600050549250600083141561132457611535565b610103600050600086815260200190815260200160002060005091506000826000016000505414156113fd57600060005054826000016000508190555060008260010160005081905550610104600050805480919060010190908154818355818115116113c3578183600052602060002091820191016113c291906113a4565b808211156113be57600081815060009055506001016113a4565b5090565b5b5050508260020160005081905550846101046000508360020160005054815481101561000257906000526020600020900160005b50819055505b8260020a90506000818360010160005054161415611534577fe1c52dc63b719ade82e8bea94cc41a0d5d28e4aaf536adb5e9cccc9ff8c1aeda3386604051808373ffffffffffffffffffffffffffffffffffffffff1681526020018281526020019250505060405180910390a16001826000016000505411151561150757610104600050610103600050600087815260200190815260200160002060005060020160005054815481101561000257906000526020600020900160005b50600090556101036000506000868152602001908152602001600020600060008201600050600090556001820160005060009055600282016000506000905550506001935061153556611533565b816000016000818150548092919060019003919050555080826001016000828282505417925050819055505b5b5b505050919050565b60006000600190505b6001600050548110156116d2575b60016000505481108015611580575060006002600050826101008110156100025790900160005b505414155b15611592578080600101915050611554565b5b60016001600050541180156115c45750600060026000506001600050546101008110156100025790900160005b5054145b156115e357600160008181505480929190600190039190505550611593565b600160005054811080156116145750600060026000506001600050546101008110156100025790900160005b505414155b8015611637575060006002600050826101008110156100025790900160005b5054145b156116cd5760026000506001600050546101008110156100025790900160005b50546002600050826101008110156100025790900160005b50819055508061010260005060006002600050846101008110156100025790900160005b5054815260200190815260200160002060005081905550600060026000506001600050546101008110156100025790900160005b50819055505b611546565b5b5090565b60006116e2336109a5565b1561177157610107600050546116f6611980565b111561171b57600061010660005081905550611710611980565b610107600050819055505b610106600050548261010660005054011015801561174757506101056000505482610106600050540111155b15611768578161010660008282825054019250508190555060019050611772565b60009050611772565b5b919050565b60006000610104600050805490509150600090505b8181101561187857610109600050600061010460005083815481101561000257906000526020600020900160005b5054815260200190815260200160002060006000820160006101000a81549073ffffffffffffffffffffffffffffffffffffffff0219169055600182016000506000905560028201600050805460018160011615610100020316600290046000825580601f1061182a5750611867565b601f0160209004906000526020600020908101906118669190611848565b808211156118625760008181506000905550600101611848565b5090565b5b5050505b806001019050805061178c565b611880611885565b5b5050565b60006000610104600050805490509150600090505b8181101561193857600060010261010460005082815481101561000257906000526020600020900160005b505414151561192a57610103600050600061010460005083815481101561000257906000526020600020900160005b50548152602001908152602001600020600060008201600050600090556001820160005060009055600282016000506000905550505b5b806001019050805061189a565b61010460005080546000825590600052602060002090810190611979919061195b565b80821115611975576000818150600090555060010161195b565b5090565b5b505b5050565b6000620151804204905061198f565b9056"

hexstring2 :: EvmHexString
hexstring2 =
  EvmHexString
    "6060604052361560275760e060020a600035046341c0e1b58114606e578063e5225381146096575b60d56000341115606c57346060908152605890600160a060020a033316907f90890809c654f11d6e72a28fa60149770a0d11ec6c92319d6ceb2bb0a4ea1a1590602090a35b565b60d5600054600160a060020a0390811633919091161415606c57600054600160a060020a0316ff5b60d5600054600160a060020a0390811633919091161415606c5760008054600160a060020a039081169190301631606082818181858883f15050505050565b00"

-- http://solidity.readthedocs.io/en/develop/solidity-by-example.html
voteHexstring :: EvmHexString
voteHexstring =
  EvmHexString
    "6060604052341561000c57fe5b604051610abd380380610abd833981016040528080518201919050505b600033600060006101000a81548173ffffffffffffffffffffffffffffffffffffffff021916908373ffffffffffffffffffffffffffffffffffffffff160217905550600160016000600060009054906101000a900473ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200190815260200160002060000181905550600090505b815181101561016b57600280548060010182816100f89190610173565b916000526020600020906002020160005b604060405190810160405280868681518110151561012357fe5b906020019060200201516000191681526020016000815250909190915060008201518160000190600019169055602082015181600101555050505b80806001019150506100db565b5b50506101d5565b8154818355818115116101a05760020281600202836000526020600020918201910161019f91906101a5565b5b505050565b6101d291905b808211156101ce57600060008201600090556001820160009055506002016101ab565b5090565b90565b6108d9806101e46000396000f3006060604052361561008c576000357c0100000000000000000000000000000000000000000000000000000000900463ffffffff1680630121b93f1461008e578063013cf08b146100ae5780632e4176cf146100f15780635c19a95c14610143578063609ff1bd146101795780639e7b8d611461019f578063a3ec138d146101d5578063e2ba53f014610264575bfe5b341561009657fe5b6100ac6004808035906020019091905050610292565b005b34156100b657fe5b6100cc6004808035906020019091905050610352565b6040518083600019166000191681526020018281526020019250505060405180910390f35b34156100f957fe5b610101610386565b604051808273ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200191505060405180910390f35b341561014b57fe5b610177600480803573ffffffffffffffffffffffffffffffffffffffff169060200190919050506103ac565b005b341561018157fe5b610189610696565b6040518082815260200191505060405180910390f35b34156101a757fe5b6101d3600480803573ffffffffffffffffffffffffffffffffffffffff1690602001909190505061071d565b005b34156101dd57fe5b610209600480803573ffffffffffffffffffffffffffffffffffffffff1690602001909190505061081c565b60405180858152602001841515151581526020018373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200182815260200194505050505060405180910390f35b341561026c57fe5b610274610879565b60405180826000191660001916815260200191505060405180910390f35b6000600160003373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200190815260200160002090508060010160009054906101000a900460ff16156102f157610000565b60018160010160006101000a81548160ff021916908315150217905550818160020181905550806000015460028381548110151561032b57fe5b906000526020600020906002020160005b50600101600082825401925050819055505b5050565b60028181548110151561036157fe5b906000526020600020906002020160005b915090508060000154908060010154905082565b600060009054906101000a900473ffffffffffffffffffffffffffffffffffffffff1681565b60006000600160003373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200190815260200160002091508160010160009054906101000a900460ff161561040d57610000565b3373ffffffffffffffffffffffffffffffffffffffff168373ffffffffffffffffffffffffffffffffffffffff16141561044657610000565b5b600073ffffffffffffffffffffffffffffffffffffffff16600160008573ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200190815260200160002060010160019054906101000a900473ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1614151561058257600160008473ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200190815260200160002060010160019054906101000a900473ffffffffffffffffffffffffffffffffffffffff1692503373ffffffffffffffffffffffffffffffffffffffff168373ffffffffffffffffffffffffffffffffffffffff16141561057d57610000565b610447565b60018260010160006101000a81548160ff021916908315150217905550828260010160016101000a81548173ffffffffffffffffffffffffffffffffffffffff021916908373ffffffffffffffffffffffffffffffffffffffff160217905550600160008473ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200190815260200160002090508060010160009054906101000a900460ff16156106795781600001546002826002015481548110151561065257fe5b906000526020600020906002020160005b5060010160008282540192505081905550610690565b816000015481600001600082825401925050819055505b5b505050565b60006000600060009150600090505b60028054905081101561071757816002828154811015156106c257fe5b906000526020600020906002020160005b50600101541115610709576002818154811015156106ed57fe5b906000526020600020906002020160005b506001015491508092505b5b80806001019150506106a5565b5b505090565b600060009054906101000a900473ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff163373ffffffffffffffffffffffffffffffffffffffff161415806107c65750600160008273ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200190815260200160002060010160009054906101000a900460ff165b156107d057610000565b6001600160008373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff168152602001908152602001600020600001819055505b50565b60016020528060005260406000206000915090508060000154908060010160009054906101000a900460ff16908060010160019054906101000a900473ffffffffffffffffffffffffffffffffffffffff16908060020154905084565b60006002610885610696565b81548110151561089157fe5b906000526020600020906002020160005b506000015490505b905600a165627a7a7230582083216429ab08775d8e6d7b7643b8ab38785e52186a5dca7d37aefaf2103eabcb0029"
