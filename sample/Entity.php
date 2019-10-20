<?php

use JMS\Serializer\Annotation as Serializer;

/**
 * Class Entity
 * 
 */
class Entity extends AbstractEntity
{
    /**
     * @var integer
     * @Serializer\Type("integer")
     * @Serializer\SerializedName("id")
     * @Serializer\Accessor(getter="getId", setter="setId")
     */
    protected $id;
    /**
     * @var integer
     * @Serializer\Type("integer")
     * 
     */
    protected $partner;
    /**
     * @var integer
     * @Serializer\Type("integer")
     * @Serializer\SerializedName("entiy_enabled")
     * @Serializer\Exclude()
     */
    protected $entityEnabled;
    
    /**
     * @return mixed
     */
    public function getId()
    {
        return $this->id;
    }

}
