module Ethereum.Analyzer.TestData.StorageJson
  ( storageJson
  ) where

import Protolude hiding (show)

storageJson :: Text
storageJson =
  "{" <>
  "  \"children\" :" <>
  "  [" <>
  "    {" <>
  "      \"attributes\" :" <>
  "      {" <>
  "        \"literals\" :" <>
  "        [" <>
  "          \"solidity\"," <>
  "          \"^\"," <>
  "          \"0.4\"," <>
  "          \".0\"" <>
  "        ]" <>
  "      }," <>
  "      \"id\" : 1," <>
  "      \"name\" : \"PragmaDirective\"," <>
  "      \"src\" : \"0:23:-1\"" <>
  "    }," <>
  "    {" <>
  "      \"attributes\" :" <>
  "      {" <>
  "        \"fullyImplemented\" : true," <>
  "        \"isLibrary\" : false," <>
  "        \"linearizedBaseContracts\" :" <>
  "        [" <>
  "          22" <>
  "        ]," <>
  "        \"name\" : \"SimpleStorage\"" <>
  "      }," <>
  "      \"children\" :" <>
  "      [" <>
  "        {" <>
  "          \"attributes\" :" <>
  "          {" <>
  "            \"constant\" : false," <>
  "            \"name\" : \"storedData\"," <>
  "            \"storageLocation\" : \"default\"," <>
  "            \"type\" : \"uint256\"," <>
  "            \"visibility\" : \"internal\"" <>
  "          }," <>
  "          \"children\" :" <>
  "          [" <>
  "            {" <>
  "              \"attributes\" :" <>
  "              {" <>
  "                \"name\" : \"uint\"" <>
  "              }," <>
  "              \"id\" : 2," <>
  "              \"name\" : \"ElementaryTypeName\"," <>
  "              \"src\" : \"52:4:-1\"" <>
  "            }" <>
  "          ]," <>
  "          \"id\" : 3," <>
  "          \"name\" : \"VariableDeclaration\"," <>
  "          \"src\" : \"52:15:-1\"" <>
  "        }," <>
  "        {" <>
  "          \"attributes\" :" <>
  "          {" <>
  "            \"constant\" : false," <>
  "            \"name\" : \"set\"," <>
  "            \"payable\" : false," <>
  "            \"visibility\" : \"public\"" <>
  "          }," <>
  "          \"children\" :" <>
  "          [" <>
  "            {" <>
  "              \"children\" :" <>
  "              [" <>
  "                {" <>
  "                  \"attributes\" :" <>
  "                  {" <>
  "                    \"constant\" : false," <>
  "                    \"name\" : \"x\"," <>
  "                    \"storageLocation\" : \"default\"," <>
  "                    \"type\" : \"uint256\"," <>
  "                    \"visibility\" : \"internal\"" <>
  "                  }," <>
  "                  \"children\" :" <>
  "                  [" <>
  "                    {" <>
  "                      \"attributes\" :" <>
  "                      {" <>
  "                        \"name\" : \"uint\"" <>
  "                      }," <>
  "                      \"id\" : 4," <>
  "                      \"name\" : \"ElementaryTypeName\"," <>
  "                      \"src\" : \"85:4:-1\"" <>
  "                    }" <>
  "                  ]," <>
  "                  \"id\" : 5," <>
  "                  \"name\" : \"VariableDeclaration\"," <>
  "                  \"src\" : \"85:6:-1\"" <>
  "                }" <>
  "              ]," <>
  "              \"id\" : 6," <>
  "              \"name\" : \"ParameterList\"," <>
  "              \"src\" : \"84:8:-1\"" <>
  "            }," <>
  "            {" <>
  "              \"children\" : []," <>
  "              \"id\" : 7," <>
  "              \"name\" : \"ParameterList\"," <>
  "              \"src\" : \"93:0:-1\"" <>
  "            }," <>
  "            {" <>
  "              \"children\" :" <>
  "              [" <>
  "                {" <>
  "                  \"children\" :" <>
  "                  [" <>
  "                    {" <>
  "                      \"attributes\" :" <>
  "                      {" <>
  "                        \"operator\" : \"=\"," <>
  "                        \"type\" : \"uint256\"" <>
  "                      }," <>
  "                      \"children\" :" <>
  "                      [" <>
  "                        {" <>
  "                          \"attributes\" :" <>
  "                          {" <>
  "                            \"type\" : \"uint256\"," <>
  "                            \"value\" : \"storedData\"" <>
  "                          }," <>
  "                          \"id\" : 8," <>
  "                          \"name\" : \"Identifier\"," <>
  "                          \"src\" : \"99:10:-1\"" <>
  "                        }," <>
  "                        {" <>
  "                          \"attributes\" :" <>
  "                          {" <>
  "                            \"type\" : \"uint256\"," <>
  "                            \"value\" : \"x\"" <>
  "                          }," <>
  "                          \"id\" : 9," <>
  "                          \"name\" : \"Identifier\"," <>
  "                          \"src\" : \"112:1:-1\"" <>
  "                        }" <>
  "                      ]," <>
  "                      \"id\" : 10," <>
  "                      \"name\" : \"Assignment\"," <>
  "                      \"src\" : \"99:14:-1\"" <>
  "                    }" <>
  "                  ]," <>
  "                  \"id\" : 11," <>
  "                  \"name\" : \"ExpressionStatement\"," <>
  "                  \"src\" : \"99:14:-1\"" <>
  "                }" <>
  "              ]," <>
  "              \"id\" : 12," <>
  "              \"name\" : \"Block\"," <>
  "              \"src\" : \"93:25:-1\"" <>
  "            }" <>
  "          ]," <>
  "          \"id\" : 13," <>
  "          \"name\" : \"FunctionDefinition\"," <>
  "          \"src\" : \"72:46:-1\"" <>
  "        }," <>
  "        {" <>
  "          \"attributes\" :" <>
  "          {" <>
  "            \"constant\" : true," <>
  "            \"name\" : \"get\"," <>
  "            \"payable\" : false," <>
  "            \"visibility\" : \"public\"" <>
  "          }," <>
  "          \"children\" :" <>
  "          [" <>
  "            {" <>
  "              \"children\" : []," <>
  "              \"id\" : 14," <>
  "              \"name\" : \"ParameterList\"," <>
  "              \"src\" : \"134:2:-1\"" <>
  "            }," <>
  "            {" <>
  "              \"children\" :" <>
  "              [" <>
  "                {" <>
  "                  \"attributes\" :" <>
  "                  {" <>
  "                    \"constant\" : false," <>
  "                    \"name\" : \"\"," <>
  "                    \"storageLocation\" : \"default\"," <>
  "                    \"type\" : \"uint256\"," <>
  "                    \"visibility\" : \"internal\"" <>
  "                  }," <>
  "                  \"children\" :" <>
  "                  [" <>
  "                    {" <>
  "                      \"attributes\" :" <>
  "                      {" <>
  "                        \"name\" : \"uint\"" <>
  "                      }," <>
  "                      \"id\" : 15," <>
  "                      \"name\" : \"ElementaryTypeName\"," <>
  "                      \"src\" : \"155:4:-1\"" <>
  "                    }" <>
  "                  ]," <>
  "                  \"id\" : 16," <>
  "                  \"name\" : \"VariableDeclaration\"," <>
  "                  \"src\" : \"155:4:-1\"" <>
  "                }" <>
  "              ]," <>
  "              \"id\" : 17," <>
  "              \"name\" : \"ParameterList\"," <>
  "              \"src\" : \"154:6:-1\"" <>
  "            }," <>
  "            {" <>
  "              \"children\" :" <>
  "              [" <>
  "                {" <>
  "                  \"children\" :" <>
  "                  [" <>
  "                    {" <>
  "                      \"attributes\" :" <>
  "                      {" <>
  "                        \"type\" : \"uint256\"," <>
  "                        \"value\" : \"storedData\"" <>
  "                      }," <>
  "                      \"id\" : 18," <>
  "                      \"name\" : \"Identifier\"," <>
  "                      \"src\" : \"174:10:-1\"" <>
  "                    }" <>
  "                  ]," <>
  "                  \"id\" : 19," <>
  "                  \"name\" : \"Return\"," <>
  "                  \"src\" : \"167:17:-1\"" <>
  "                }" <>
  "              ]," <>
  "              \"id\" : 20," <>
  "              \"name\" : \"Block\"," <>
  "              \"src\" : \"161:28:-1\"" <>
  "            }" <>
  "          ]," <>
  "          \"id\" : 21," <>
  "          \"name\" : \"FunctionDefinition\"," <>
  "          \"src\" : \"122:67:-1\"" <>
  "        }" <>
  "      ]," <>
  "      \"id\" : 22," <>
  "      \"name\" : \"ContractDefinition\"," <>
  "      \"src\" : \"25:166:-1\"" <>
  "    }" <>
  "  ]," <>
  "  \"name\" : \"SourceUnit\"" <>
  "}"
