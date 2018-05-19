#if !defined(MESSAGES_INCLUDED_)
#define MESSAGES_INCLUDED_

//监听机制中用到的消息定义

#include "InterfaceObj.h"

namespace easymap
{

//消息大类
const event_identity    MESSAGE_VECTORLAYER_                = 1000;
const event_identity    MESSAGE_VECTORFEATURE_              = 1100;

//---------------------------------------------------------------------
//  用于SlimLayer和ShapeLayer作为被监听者
//---------------------------------------------------------------------
//  新增加要素，传出新增加要素的fid
const event_identity    MESSAGE_VECTORLAYER_ADDED           = MESSAGE_VECTORLAYER_ + 1;
//  要素被删除，传出被删除要素的fid
const event_identity    MESSAGE_VECTORLAYER_DELETED         = MESSAGE_VECTORLAYER_ + 2;
//  要素已经被修改，传出被修改的要素的fid
const event_identity    MESSAGE_VECTORLAYER_MODIFIED        = MESSAGE_VECTORLAYER_ + 3;
//  undo
const event_identity    MESSAGE_VECTORLAYER_UNDO            = MESSAGE_VECTORLAYER_ + 4;
//  redo
const event_identity    MESSAGE_VECTORLAYER_REDO            = MESSAGE_VECTORLAYER_ + 5;
//  编辑被保存
const event_identity    MESSAGE_VECTORLAYER_EDITSAVED       = MESSAGE_VECTORLAYER_ + 6;
//  编辑被撤销
const event_identity    MESSAGE_VECTORLAYER_EDITCANCELED    = MESSAGE_VECTORLAYER_ + 7;
//  图层被别的用户修改，索引已被刷新
const event_identity    MESSAGE_VECTORLAYER_INDEXRELOADED   = MESSAGE_VECTORLAYER_ + 8;
//---------------------------------------------------------------------


//---------------------------------------------------------------------
//  用于VectorFeature作为被监听者
//---------------------------------------------------------------------
//  通知监听者，Feature::Update()已经被调用，新创建的Feature
const event_identity    MESSAGE_VECTORFEATURE_ADDED         = MESSAGE_VECTORFEATURE_ + 1;

//  通知监听者，Feature::Update()已经被调用，修改的Feature
const event_identity    MESSAGE_VECTORFEATURE_MODIFIED      = MESSAGE_VECTORFEATURE_ + 2;
//---------------------------------------------------------------------


}

#endif
