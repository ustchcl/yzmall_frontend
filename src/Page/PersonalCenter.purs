module Yzmall.Page.PersonalCenter where

import Halogen.Themes.Bootstrap4
import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Yzmall.Data.Account (Account)
import Yzmall.Utils (cls, style, (<+>), (->>))


type State = Maybe Account

type ItemConfig = Tuple String (Maybe String)

data Query a
  = Initialize a

component :: forall m. Maybe Account -> H.Component HH.HTML Query Unit Void m
component account =
    H.component
        { initialState: const account
        , render
        , eval 
        , receiver: const Nothing
        }
    where
      eval :: Query ~> H.ComponentDSL State Query Void m
      eval = case _ of
        Initialize a -> do
          pure a

      render :: State -> H.ComponentHTML Query
      render _ = 
        HH.div
        [ style "background-color: #EFEFF3"
        , cls $ w100 ]
        (renderListGroup <$> itemNames)
      
      itemNames :: Array (Array ItemConfig)
      itemNames = 
        [ [ Tuple "填写邮寄地址" Nothing ]
        , [ Tuple "余额" $ Just "0.00"
          , Tuple "金币数" $ Just "0"
          ]
        , [ Tuple "我的订单" Nothing ]
        , [ Tuple "系统公告" Nothing
          , Tuple "消息通知" Nothing
          , Tuple "问题反馈" Nothing
          ]
        , [ Tuple "关于我们" $ Just "1.0.10"
          , Tuple "退换货政策" Nothing
          , Tuple "购物流程" Nothing
          , Tuple "保定祥琴网络科技有限公司® 版权所有" Nothing
          ]
        ]
      

      


renderListGroup :: forall p i. Array ItemConfig -> H.HTML p i
renderListGroup arr =
  HH.div
  [ cls $ listGroup <> mb3 <> mxAuto
  , style "max-width: 600px" ]
  (renderItem <$> arr)



renderItem :: forall p i. ItemConfig -> H.HTML p i
renderItem  (Tuple key value) =
  HH.a
  [ cls $ listGroupItem <> listGroupItemAction <> listGroupItemLight <> dFlex <> alignItemsCenter <> px4 <> borderBottom <> rounded0
  , style "color: #5c5c5c"
  , HP.href "#"
  ]
  [ 
    HH.div_
    [ --HH.i
    --   [ "class" ->> "fas fa-yen-sign" ]
    --   []
    -- , 
    HH.text key
    ]
  , renderRight value
  ]
  where
  renderRight str =
    case str of
      Just v ->
        HH.div
        [ cls mlAuto]
        [ HH.text v
        , HH.i
          [ cls $ ml2 <+> "fas fa-chevron-right" ]
          []
        ]
      Nothing ->
        HH.i
        [ cls $ mlAuto <+> "fas fa-chevron-right" ]
        []


renderItem2 :: forall p i. H.HTML p i
renderItem2 = 
  HH.a
  [ cls $ listGroupItem <> listGroupItemAction <> listGroupItemDark <> dFlex <> alignItemsCenter <> px2 <> border0 <> borderBottom <> mb3 <> rounded0
  , HP.href "#"
  ]
  [ HH.div_
    [ HH.i
      [ "class" ->> "fas fa-yen-sign" ]
      []
    , HH.text "some thing"
    ]
  , HH.i
    [ cls $ mlAuto <+> "fas fa-chevron-right" ]
    []
  ]
